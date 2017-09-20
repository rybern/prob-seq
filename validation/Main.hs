{-# LANGUAGE OverloadedLists #-}
module Main where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List
import Control.Monad
import Data.Monoid
import Data.Maybe
import Sequence
import System.Process
import Data.Function
import qualified Data.Map as M

import EmissionIO

main :: IO ()
main = do
  let genSeq = buildMatSeq $ repeatSequence 15 periodSeq
      priorSeq = buildMatSeq $ andThen
                    (repeatSequence 0 periodSeq)
        (uniformDistRepeat 20 periodSeq)
      f = getRepeat
  --let genSeq = buildMatSeq $ uniformDistRepeat 10 (finiteDistRepeat [0, 0.5, 0.5] (deterministicSequence [1]))
      --priorSeq = genSeq
      --f (_, (_, StateTag n _)) = Just n
  compareHMM genSeq priorSeq f

compareHMM :: (Show s, Eq s)
           => MatSeq s
           -> MatSeq s
           -> ((Prob, (s, StateTag)) -> Maybe Int)
           -> IO ()
compareHMM genSeq priorSeq f = do
  (sampleIxs, posterior, viterbi, forward, backward) <- runHMM genSeq priorSeq
  let sample = V.map (fst . (stateLabels genSeq V.!)) sampleIxs

  putStrLn $ "state labels:" ++ show sample
  putStrLn $ "state generating index:" ++ show sampleIxs

  putStrLn $ "truth: " ++ show (fromMaybe 0 $ f (undefined, stateLabels priorSeq V.! V.last sampleIxs))
  putStrLn $ "viterbi: " ++ show (fromMaybe 0 $ f (undefined, stateLabels priorSeq V.! V.last viterbi))
  let tags = pathProbs (stateLabels priorSeq) posterior
      post_dist = (distOver f $ V.last tags)
      max_post = fst $ maximumBy (compare `on` snd) post_dist
  putStrLn $ "max posterior: " ++ show max_post
  putStrLn "posterior dist: "
  mapM_ (\(ix, p) -> putStrLn $ show ix ++ ": " ++ show (fromRational p)) (distOver f $ V.last tags)
  let tags = pathProbs (stateLabels priorSeq) forward
      post_dist = (distOver f $ V.last tags)
      max_post = fst $ maximumBy (compare `on` snd) post_dist
  --putStrLn $ "max forward: " ++ show max_post
  --putStrLn "forward dist: "
  --mapM_ (\(ix, p) -> putStrLn $ show ix ++ ": " ++ show (fromRational p)) (distOver f $ V.last tags)
  let tags = pathProbs (stateLabels priorSeq) backward
      post_dist = (distOver f $ V.last tags)
      max_post = fst $ maximumBy (compare `on` snd) post_dist
  --putStrLn $ "max backward: " ++ show max_post
  --putStrLn "backward dist: "
  --mapM_ (\(ix, p) -> putStrLn $ show ix ++ ": " ++ show (fromRational p)) (distOver f $ V.last tags)
  --print priorSeq

  return ()

compareIntHMM = do
  (sampleIxs, posterior, viterbi, forward, backward) <- runHMM cycleMatSeq' cycleMatSeq
  let sample = V.map (fst . (stateLabels cycleMatSeq V.!)) sampleIxs
      tags = pathProbs (stateLabels cycleMatSeq) posterior

  putStrLn $ "state labels:" ++ show sample
  putStrLn $ "state generating index:" ++ show sampleIxs
  putStrLn $ "truth: " ++ show (fromMaybe 0 $ getRepeat (undefined, stateLabels cycleMatSeq V.! V.last sampleIxs))
  putStrLn $ "viterbi: " ++ show (fromMaybe 0 $ getRepeat (undefined, stateLabels cycleMatSeq V.! V.last viterbi))
  let post_dist = (distOverRepeat $ V.last tags)
      max_post = fst $ maximumBy (compare `on` snd) post_dist
  putStrLn $ "max posterior: " ++ show max_post
  putStrLn "posterior dist: "
  mapM_ (\(ix, p) -> putStrLn $ show (succ ix) ++ ": " ++ show (fromRational p)) (distOverRepeat $ V.last tags)

  return ()


runHMM :: (Show s, Eq s) => MatSeq s -> MatSeq s -> IO (Vector Int, Emissions, Vector Int, Emissions, Emissions)
runHMM genSeq priorSeq = do
  sampleIxs <- fst <$> sampleSeqIxs vecDist genSeq
  --let sampleIxs = [0,1,2,5,6,7,9,15,16,24,25,26,29]

  let sample = V.map (fst . (stateLabels genSeq V.!)) sampleIxs

  let transFile = "cycle_sim_transitions.st"
      emissionsFile = "cycle_sim_emissions.csv"
      posteriorFile = "cycle_sim_posterior.csv"
      viterbiFile = "cycle_sim_viterbi.csv"
      forwardFile = "cycle_sim_forward.csv"
      backwardFile = "cycle_sim_backward.csv"

  writeSTFile (mapStates show priorSeq) transFile
  emissions <- mapM (obsProb priorSeq) sample
  writeEmissions emissions emissionsFile

  runPomegranate transFile emissionsFile posteriorFile viterbiFile forwardFile backwardFile

  (Right posterior) <- readEmissions posteriorFile
  (Right viterbiStateIxs) <- readViterbi viterbiFile
  (Right forward) <- readEmissions forwardFile
  (Right backward) <- readEmissions backwardFile

  return (sampleIxs, posterior, viterbiStateIxs, forward, backward)

type StateDist s = V.Vector (Prob, (s, StateTag))

removeImpossible :: StateDist s -> StateDist s
removeImpossible = V.filter ((/=0) . fst)

maxPosterior :: StateDist s -> (Prob, (s, StateTag))
maxPosterior = maximumBy (compare `on` fst) . V.toList

distOverRepeat :: (Ord s) => StateDist s -> [(Int, Prob)]
distOverRepeat = distOver getRepeat

getRepeat :: ((Prob, (s, StateTag)) -> Maybe Int)
getRepeat (_, (_, StateTag 1 [StateTag n _])) = Just n
getRepeat _ = Nothing

distOverLabel :: (Ord s) => StateDist s -> [(s, Prob)]
distOverLabel = distOver (Just . fst . snd)

distOver :: (Ord b) => ((Prob, (s, StateTag)) -> Maybe b) -> StateDist s -> [(b, Prob)]
distOver f dist = map (\(b, groupDist) -> (b, sumDist groupDist)) (groupOn f dist)

sumDist :: (Foldable t, Functor t) => t (Prob, (s, StateTag)) -> Prob
sumDist = sum . fmap fst

groupOn :: (Foldable t, Ord b) => (a -> Maybe b) -> t a -> [(b, [a])]
groupOn f t = M.toList $ foldl
              (\m a -> maybe
                m
                (\b -> M.insertWith (++) b [a] m)
                (f a))
              []
              t

pathProbs :: V.Vector (s, StateTag) -> Emissions -> V.Vector (V.Vector (Prob, (s, StateTag)))
pathProbs stateLabels = V.map (flip V.zip stateLabels)

runPomegranate :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runPomegranate trans emms post viti fwd bwd= do
  callProcess "/run/current-system/sw/bin/python"
    ["/home/ryan/documents/haskell/smol/prob-seq/validation/pomegratate/build_hmm.py"
    , trans, emms, post, viti, fwd, bwd ]
  return ()

normalize :: Vector Prob -> Vector Prob
normalize v = let s = sum v in (/ s) <$> v

obsProb :: (Eq s) => MatSeq s -> s -> IO (Vector Prob)
obsProb seq i = return . normalize . V.map (\(i', _) -> if i == i' then 1.0 else 0.0) . stateLabels $ seq

sampleCycleMatIxs :: IO (Vector Int)
sampleCycleMatIxs = fst <$> sampleSeqIxs vecDist cycleMatSeq

sampleCycleMatIxs' :: IO (Vector Int)
sampleCycleMatIxs' = fst <$> sampleSeqIxs vecDist cycleMatSeq'

sampleCycleMat :: IO (Vector Int)
sampleCycleMat = fst <$> sampleSeq vecDist cycleMatSeq

cycleMatSeq :: MatSeq Int
cycleMatSeq = buildMatSeq cycleSeq

cycleMatSeq' :: MatSeq Int
cycleMatSeq' = buildMatSeq cycleSeq'

cycleSeq' :: ProbSeq Int
cycleSeq' = repeatSequence n periodSeq

n = 15

cycleSeq :: ProbSeq Int
cycleSeq = andThen
  (repeatSequence nStart periodSeq)
  (uniformDistRepeat nEnd periodSeq)
  where nStart = 0
        nEnd = 20

periodSeq :: ProbSeq Int
periodSeq = series . map (\v -> andThen (deterministicSequence . V.singleton $ v) skipDistSeq) $
  [ 2, 1 ]

skipDist :: [Prob]
skipDist = [0.5, 1 - head skipDist]

skipDistSeq :: ProbSeq a
skipDistSeq = finiteDistRepeat skipDist $ skip 1
