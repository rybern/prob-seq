{-# LANGUAGE OverloadedLists #-}
module Main where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List
import Control.Monad
import Data.Monoid
import Sequence
import System.Process

import EmissionIO

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

bif 0 = 2
bif 1 = 2
bif n = bif (n-1) + bif (n-2)

{-
without pseq, -O0

[1,2,1,1,1,1,2,2]

real	0m24.409s
user	0m26.005s
sys	0m1.939s
-}

main :: IO ()
main = do
  sample <- sampleCycleMat

  print sample

  emissions <- mapM (obsProb cycleMatSeq) sample

  let transFile = "cycle_sim_transitions.st"
      emissionsFile = "cycle_sim_emissions.csv"
      posteriorFile = "cycle_sim_posterior.csv"
      viterbiFile = "cycle_sim_viterbi.csv"

  writeSTFile (mapStates show cycleMatSeq) transFile
  writeEmissions emissions emissionsFile

  runPomegranate transFile emissionsFile posteriorFile viterbiFile

  posterior <- readEmissions posteriorFile
  (Right viterbiStateIxs) <- readViterbi viterbiFile
  let (viterbiLabels, viterbiTags) = V.unzip $ fmap (stateLabels cycleMatSeq V.!) viterbiStateIxs

  print viterbiLabels
  print $ V.last viterbiTags

  return ()

--callProcess "python /home/ryan/documents/haskell/smol/prob-seq/validation/pomegratate/build_hmm.py" []

--callProcess "/run/current-system/sw/bin/python" ["/home/ryan/documents/haskell/smol/prob-seq/validation/pomegratate/build_hmm.py"]

runPomegranate :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runPomegranate trans emms post viti = do
  callProcess "/run/current-system/sw/bin/python" ["/home/ryan/documents/haskell/smol/prob-seq/validation/pomegratate/build_hmm.py", trans, emms, post, viti]
  return ()

writeEmissionsFile :: Vector (Vector Prob) -> FilePath -> IO ()
writeEmissionsFile ems fp = writeFile fp . unlines . V.toList . V.map line $ ems
  where line = concat . intersperse "\t" . V.toList . V.map (show . fromRational)

normalize :: Vector Prob -> Vector Prob
normalize v = let s = sum v in (/ s) <$> v

obsProb :: MatSeq Int -> Int -> IO (Vector Prob)
obsProb seq i = return . normalize . V.map (\(i', _) -> if i == i' then 1.0 else 0.0) . stateLabels $ seq

sampleCycleMat :: IO (Vector Int)
sampleCycleMat = fst <$> sampleSeq vecDist cycleMatSeq

cycleMatSeq :: MatSeq Int
cycleMatSeq = buildMatSeq cycleSeq

cycleSeq :: ProbSeq Int
cycleSeq = Fix $ AndThen
  (Fix . Repeat nStart $ periodSeq)
  (Fix . FiniteDistRepeat (replicate nEnd (1/fromIntegral nEnd)) $ periodSeq)
  where nStart = 10
        nEnd = 10

periodSeq :: ProbSeq Int
periodSeq = Fix . Series . map (\v -> Fix $ AndThen (Fix . DeterministicSequence . V.singleton $ v) skipDistSeq) $
  [ 1, 2, 2, 1 ]

series :: [ProbSeq a] -> ProbSeq a
series = foldl1' (\a b -> Fix $ AndThen a b)

skipDistSeq :: ProbSeq a
skipDistSeq = Fix $ Possibly 0.5 (Fix $ Skip 1)

skipDist :: [Prob]
skipDist = [0.4, 0.3, 0.2, 0.1]

skipDistSeq' :: ProbSeq a
skipDistSeq' = Fix . Possibly (1 - head skipDist) . Fix . FiniteDistRepeat (tail skipDist) . Fix $ Skip 1
