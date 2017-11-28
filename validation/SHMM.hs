module SHMM where

import Data.Map (Map)
import qualified Data.Map as Map
import EmissionIO
import Sequence
import System.Process
import System.IO.Temp
import Data.Monoid
import EmissionPermutation

runHMM :: Map String Int
       -> FilePath
       -> MatSeq String
       -> IO Emissions
runHMM indexMap emissionsFile priorSeq = do
  withSystemTempDirectory "" $ \dir -> do
    --let --transFile = dir<>"/transitions.st"
        --permutationFile = dir<>"/permutation.csv"
        --posteriorFile = dir<>"/posterior.csv"


    let transFile = "minion_SHMM_trans.st"
    let permutationFile = "minion_SHMM_permutation.csv"
    let posteriorFile = "minion_SHMM_posterior.csv"

    writeSTFile priorSeq transFile
    writeEmissionPerm indexMap permutationFile priorSeq

    callSHMM transFile emissionsFile permutationFile posteriorFile

    (Right posterior) <- readEmissions posteriorFile

    return posterior

callSHMM :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
callSHMM trans emms perm post = do
  callProcess "./shmm" [ trans, emms, post, perm ]
  return ()
