{-# LANGUAGE OverloadedLists, ViewPatterns #-}
module Main where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List
import Control.Monad
import Data.Monoid
import Data.Maybe
import Sequence
import Data.Function
import System.Environment

import EmissionIO
import Utils
import Pomegranate

main = do
  [_, [ref], [alt], maf', leftFlank, [ref'], rightFlank] <- getArgs

  --write this as a configuration file
  print maf'
  let maf = toRational $ (read maf' :: Double)
  let snp = eitherOr maf (state [alt]) (state [ref])
  let region = series $ map (state . return) leftFlank ++ [snp] ++ map (state . return) rightFlank
  let noise = geometricRepeat 0.5 (uniformDistOver [state "G", state "A", state "T", state "C"])
  let seq = series [noise, region, noise]

  let ps = minion seq

  let ms = buildMatSeq ps
  print =<< sampleSeq vecDist ms
  return ()

minion :: ProbSeq [s] -> ProbSeq [s]
minion = skipDist [0.4, 0.25, 0.15, 0.1, 0.1] . collapse undefined concat 4

--main = compareSmall

  {-
compareMicrosatellites :: IO ()
compareMicrosatellites = do
  let satellite :: ProbSeq String
      satellite = series . map (\c -> state [c]) $ "ATTTA"
      genSeq = buildMatSeq . minion $ repeatSequence 15 satellite
      priorSeq = buildMatSeq . minion $ andThen
                    (repeatSequence 10 satellite)
                    (uniformDistRepeat 20 satellite)
      getRepeat :: ((Prob, (s, StateTag)) -> Maybe Int)
      getRepeat (_, (_, StateTag 1 [StateTag n _])) = Just n
      getRepeat _ = Nothing
  print =<< sampleSeq vecDist priorSeq
  print $ V.head $ stateLabels priorSeq

  compareHMM genSeq priorSeq getRepeat

compareSmall :: IO ()
compareSmall = do
  let genSeq = buildMatSeq $ repeatSequence 15 periodSeq
      priorSeq = buildMatSeq $ andThen
                    (repeatSequence 0 periodSeq)
        (uniformDistRepeat 20 periodSeq)
      f = getRepeat
  compareHMM genSeq priorSeq f

getRepeat :: ((Prob, (s, StateTag)) -> Maybe Int)
getRepeat (_, (_, StateTag 1 [StateTag n _])) = Just n
getRepeat _ = Nothing

compareHMM :: (Show s, Eq s)
           => MatSeq s
           -> MatSeq s
           -> ((Prob, (s, StateTag)) -> Maybe Int)
           -> IO ()
compareHMM genSeq priorSeq f = do
  (V.unzip -> (sample, sampleIxs), posterior, viterbi, forward, backward) <- runHMM genSeq priorSeq obsProb

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
-}

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
periodSeq = series' . map (\v -> andThen (state v) skipDSeq) $
  [ 2, 1 ]

skipD :: [Prob]
skipD = [0.5, 1 - head skipD]

skipDSeq :: ProbSeq a
skipDSeq = finiteDistRepeat skipD $ skip 1
