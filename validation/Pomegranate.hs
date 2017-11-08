{-# LANGUAGE ViewPatterns #-}
module Pomegranate where

import System.Process
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Monoid
import EmissionIO
import Sequence
import System.IO.Temp

data PomResults s = PomResults {
    emissions :: Emissions
  , posterior :: Emissions
  , forward :: Emissions
  , backward :: Emissions
  , viterbi :: Vector Int
  }

runHMMSample :: (Show s, Eq s)
              => MatSeq s
              -> MatSeq s
              -> (MatSeq s -> s -> IO (Vector Prob))
              -> IO (PomResults s, Vector (s, Int), MatSeq s, MatSeq s)
runHMMSample genSeq priorSeq obsProb = do
  sampleIxs <- fst <$> sampleSeqIxs vecDist genSeq
  let sample = V.map (fst . (stateLabels genSeq V.!)) sampleIxs
  emissions <- mapM (obsProb priorSeq) sample

  res <- runHMM emissions priorSeq

  return (res, V.zip sample sampleIxs, priorSeq, genSeq)

runHMM :: (Show s, Eq s)
       => Emissions
       -> MatSeq s
       -> IO (PomResults s)
runHMM emissions priorSeq = do
  withSystemTempDirectory "" $ \dir -> do
    let transFile = dir<>"/cycle_sim_transitions.st"
        emissionsFile = dir<>"/cycle_sim_emissions.csv"
        posteriorFile = dir<>"/cycle_sim_posterior.csv"
        viterbiFile = dir<>"/cycle_sim_viterbi.csv"
        forwardFile = dir<>"/cycle_sim_forward.csv"
        backwardFile = dir<>"/cycle_sim_backward.csv"

    writeSTFile (mapStates show priorSeq) transFile
    writeEmissions emissions emissionsFile

    callPomegranate transFile emissionsFile posteriorFile viterbiFile forwardFile backwardFile

    (Right posterior) <- readEmissions posteriorFile
    (Right viterbiStateIxs) <- readViterbi viterbiFile
    (Right forward) <- readEmissions forwardFile
    (Right backward) <- readEmissions backwardFile

    putStr "'"

    return $ PomResults emissions posterior forward backward viterbiStateIxs


callPomegranate :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
callPomegranate trans emms post viti fwd bwd = do
  callProcess "/run/current-system/sw/bin/python"
    ["/home/ryan/documents/haskell/smol/prob-seq/validation/pomegratate/build_hmm.py"
    , trans, emms, post, viti, fwd, bwd ]
  return ()
