{-# LANGUAGE BangPatterns, RecordWildCards, OverloadedLists, ViewPatterns #-}
module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List
import Control.Monad
import Data.Monoid
import Data.Maybe
import Sequence
import Data.Function
import System.Environment
import System.IO

import EmissionPermutation
import EmissionIO
import Utils
import Pomegranate
import qualified SHMM as SHMM

main = do
  -- [_, [ref], [alt], maf', leftFlank, [ref'], rightFlank] <- getArgs
  --let row = "62525667 T C 0.3954 CCTTGGATGC T ACTGGGTTTG"
  --let row = "62526024 T G 0.4101 CGCTCCAGGG T TGAGCAAATG"
  --let args = words "deamer.fast5.nanonet_posterior.csv 62525667 T C 0.3954 CCTTGGATGC T ACTGGGTTTG"
  args <- getArgs

  (emissionsFile, siteLines, outputFile) <- case args of
    [emissionsFile, sitesFile, outputFile] -> do
      siteLines <- lines <$> readFile sitesFile
      return (emissionsFile, siteLines, outputFile)
    otherwise -> do
      hPutStrLn stderr "Arguments invalid, using test arguments"
      return ("minion_post.csv", testLines, "snp-calling-test-output.csv")

  let sites = map parseSite siteLines

  h <- openFile outputFile WriteMode
  -- running out of memory!
  calls <- forM sites $ \site -> do
    hPutStrLn stderr "attempting site"
    p <- callSNP emissionsFile site
    let res = show (pos site) ++ "," ++ show (fromRational (maf site)) ++ "," ++ show (fromRational p)
    hPutStrLn h res

  --writeFile outputFile $ unlines calls

  return ()

data Site = Site {
    pos :: Int
  , ref :: Char
  , alt :: Char
  , maf :: Prob
  , leftFlank :: String
  , ref' :: Char
  , rightFlank :: String
  }

parseSite :: String -> Site
parseSite row = Site {
    pos = pos
  , ref = ref
  , alt = alt
  , maf = maf
  , leftFlank = leftFlank
  , ref' = ref'
  , rightFlank = rightFlank
  }
  where
    [read -> pos, [ref], [alt], toRational . (read::String->Double) -> maf, leftFlank, [ref'], rightFlank] = words row

callSNP :: FilePath
        -> Site
        -> IO Prob
callSNP emissionsFile site = do
  let (ms, refIxs, altIxs) = snpMatSeq site
  --write this as a configuration file
  --print =<< sampleSeq vecDist ms
  --writeEmissionPerm minionIndexMap "emission_perm.csv" ms
  --writeSTFile ms "test_ex.st"
  post <- SHMM.runHMM minionIndexMap emissionsFile ms
  let refP = stateProbs refIxs post
      altP = stateProbs altIxs post
  return (altP / (refP + altP))

stateProbs :: Vector Int -> Emissions -> Prob
stateProbs ixs emissions = sum . V.map (\row -> sum . V.map (row V.!) $ ixs) $ emissions

snpMatSeq :: Site -> (MatSeq [Char], Vector Int, Vector Int)
snpMatSeq (Site {..}) = (matSeq, refIxs, altIxs)
  where matSeq = buildMatSeq . minion $
          series [noise, series [left, eitherOr maf altState refState, right] , noise]
        altState = state [alt]
        refState = state [ref]
        left = series . map (state . return) $ leftFlank
        right = series . map (state . return) $ rightFlank
        noise = geometricRepeat 0.99 (uniformDistOver (map state keyOrder))

        tags = V.map snd . stateLabels $ matSeq
        tagSNP snp (StateTag 0 [StateTag 0 seqStates]) = flip any seqStates $ \seqState ->
          case seqState of
            (StateTag 1 [StateTag 1 [StateTag x _]]) -> x == snp
            _ -> False
        tagSNP _ _ = False

        allIxs = V.enumFromTo 0 (V.length tags - 1)
        altIxs = V.filter (\i -> tagSNP 0 $ tags V.! i) allIxs
        refIxs = V.filter (\i -> tagSNP 1 $ tags V.! i) allIxs

minion :: ProbSeq [s] -> ProbSeq [s]
minion = skipDist [0.4, 0.25, 0.15, 0.1, 0.1] . collapse undefined concat 5

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

testLines :: [String]
testLines = [
    "62525667 T C 0.3954 CCTTGGATGC T ACTGGGTTTG"
  , "62525746 G C 0.002396 CAGGAGCACC G GCCGCAGAGG"
  , "62525767 T A 0.0009984 TCTGGGAGCT T CTAGGATGGG"
  , "62525777 G C 0.09844 TCTAGGATGG G AAGTGGCCCA"
  , "62525782 G A 0.001198 GATGGGAAGT G GCCCAGGCAG"
  , "62525813 A G 0.0003994 GCAGGCCGTC A GTGAGTGGCG"
  , "62525990 G C 0.007788 TGGACAGGAA G GAAGGAAGGA"
  , "62526024 T G 0.4101 CGCTCCAGGG T TGAGCAAATG"
  , "62526065 G A 0.03035 CCGGGTGGGG G CGGGGGCGAC"
  , "62526134 T G 0.003994 ACGATTACGT T TTCTCAGTCT"
  , "62526155 G T 0.003994 TACTTAAAGC G CTGAGTAAAC"
  , "62526212 C A 0.005192 CTGCGCGGTT C CCCGCAGCAC"
  , "62526224 T C 0.03195 CCGCAGCACA T GGCGTGTCCA"
  , "62526334 C T 0.01418 CAGGGCGCCT C GGCCCCGGGC"
  , "62526352 C G 0.0003994 GGCTGTCACT C GGGACTCCGC"
  , "62526369 A G 0.000599 CCGCCCCTTC A TGGACGGAGC"
  , "62526373 A G 0.01078 CCCTTCATGG A CGGAGCCTCC"
  , "62526500 T T 0.00599 CGTGCTCGTC T CCGCTGCCGC"
  , "62526500 T T 0.00599 CGTGCTCGTC T CCGCTGCCGC"
  , "62526547 T T 0.08926 CCGCGCCCTC T GCCGCCGCCG"
  , "62526547 T T 0.08926 CCGCGCCCTC T GCCGCCGCCG"
  , "62526696 G A 0.04034 CTGCGGGTCG G GCGGGCGGAT"
  , "62526719 G A 0.08187 GCCCACGTCA G GCCCGGGCAG"
  , "62526801 G C 0.09784 CCCCCGGGCC G GGGCTGCGCG"
  , "62526815 G T 0.001398 CTGCGCGGGC G CTCGGGGCCG"
  , "62526818 C T 0.0007987 CGCGGGCGCT C GGGGCCGGAG"
  , "62526898 G A 0.002796 TGCGGGAGCC G GGCCGGGCCG"
  , "62527012 C T 0.01018 GCGGCCGCCC C CAACCCCCCG"
  , "62527231 C T 0.02157 TTTATAAAAA C ATTTGAAGCC"
  , "62527305 G A 0.09006 CTAACTTGTT G GTGTTAAGTG"
  , "62527322 T A 0.002596 AGTGTCTGGA T TAAAGACTCT"
  , "62527414 A C 0.000599 TTTCAGCTTT A TTTTTGTTTT"
  , "62527427 G A 0.001997 TTTGTTTTCC G GCTTAGGCTT"
  , "62527467 C T 0.002396 TGGTTAGACA C GTCTGCCCTT"
  ]
