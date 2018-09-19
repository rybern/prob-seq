{-# LANGUAGE RecordWildCards #-}
module Inference where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.IO.Unsafe

import Sequence
import Sequence.Matrix.ProbSeqMatrixUtils
import SparseMatrix hiding (trans)

type VecMat = Vector (Vector Prob) -- use Data.Matrix?
data Emissions d = Emissions {
    emissions :: VecMat
  , indexMap :: Map d Int
  }

type InferenceEngine res = Int -> [(Int, Int, Double)] -> VecMat -> Vector Int -> res

infer :: (Ord a, Show a)
      => InferenceEngine res
      -> Emissions a
      -> MatSeq a
      -> res
infer fn (Emissions {..}) priorSeq = fn ns triples emissions permutation
  where permutation = buildEmissionPerm indexMap priorSeq
        triples = matSeqTriples priorSeq
        ns = (nStates (trans priorSeq))

inferUnsafe :: (Ord a, Show a)
            => InferenceEngine (IO res)
            -> Emissions a
            -> MatSeq a
            -> res
inferUnsafe fn emissions priorSeq = unsafePerformIO $ infer fn emissions priorSeq

matSeqTriples :: MatSeq a
              -> [(Int, Int, Double)]
matSeqTriples = map (\((r, c), p) -> (r - 1, c - 1, p)) . tail . toAssocList . cleanTrans . trans

buildEmissionPerm :: (Ord a, Show a) => Map a Int -> MatSeq a -> V.Vector Int
buildEmissionPerm m = V.map (getIndex . fst) . stateLabels
  where getIndex k = case Map.lookup k m of
          Just ix -> ix
          Nothing -> error $ "There was a label in the model that was not in the emissions data: " ++ show k

{-
runHMMSum' :: [[Vector Int]]
           -> Map String Int
           -> Emissions
           -> MatSeq String
           -> IO [[Prob]]
runHMMSum' ixs indexMap emissions priorSeq = do
  let permutation = buildEmissionPerm indexMap priorSeq
      triples = matSeqTriples priorSeq
      ns = (nStates (trans priorSeq))
  sums <- SHMM.shmmSummed ns triples emissions permutation
  return $ map (map (sum . V.map (sums V.!))) ixs

runHMMSum :: [[Vector Int]]
          -> Map String Int
          -> Emissions
          -> MatSeq String
          -> IO [[Prob]]
runHMMSum ixs indexMap emissions priorSeq = mapIxs <$> runHMM indexMap emissions priorSeq
  where mapIxs post = map (map (\arr -> stateProbs arr post)) ixs

runHMM :: Map String Int
       -> Emissions
       -> MatSeq String
       -> IO Emissions
runHMM indexMap emissions priorSeq = SHMM.shmmFull (nStates (trans priorSeq)) triples emissions permutation
  where permutation = buildEmissionPerm indexMap priorSeq
        triples = matSeqTriples priorSeq

stateProbs :: Vector Int -> Emissions -> Prob
stateProbs ixs emissions = sum . V.map (\row -> sum . V.map (row V.!) $ ixs) $ emissions

writePosts :: (FilePath, MatSeq (StateTree GenSNPState), Int, Int, Int, Emissions, [Site NT])
           -> IO ()
writePosts (outputFile, genMS, numEvents, emissionsStart, emissionsEnd, emissions, sites) = do
  forM_ sites $ \site -> do
    print $ siteEmissionsBounds softFlank numEvents emissionsStart emissionsEnd site
    let locusStr = show (pos site)
        baseFile = "post_deamer_" ++ locusStr
        csvFile = baseFile ++ ".csv"
        pngFile = baseFile ++ ".png"
        f = id -- (/ log 1000000) . log . (+ 1) . (1000000 *)
        emissions' = siteEmissions softFlank numEvents emissionsStart emissionsEnd emissions site
    writeSNPPost genMS numEvents emissionsStart emissionsEnd emissions (head sites) csvFile
    hPutStrLn stderr $ "deamer " ++ show (pos site) ++ ".[csv,png]"
  hPutStrLn stderr $ "done writing post"

main''' = do
  args <- getArgs

  let (flipped, regionFile, emissionsFile, referenceFile, outputFile) = case args of
        [flipped, regF, emiF, refF, outF] -> (flipped == "--flip", regF, emiF, refF, outF)
        _ -> let dir = "test_data_bwd1/"
                 regionFile = dir ++ "region.csv"
                 emissionsFile = dir ++ "minion_post.csv"
                 referenceFile = dir ++ "reference.txt"
                 outputFile = dir ++ "output.csv"
             in (True, regionFile, emissionsFile, referenceFile, outputFile)

  (_, emissionsStart, emissionsEnd) <- readRegionFile regionFile
  (Right emissions) <- readEmissions emissionsFile

  let (minionIndexMap', emissions') = addToken (1e-6) "_" minionIndexMap emissions

  ref <- readReference False referenceFile


  let edgeFlankSize = 50
      sites = testSNPs edgeFlankSize 100 emissionsStart ref
      model = fullSNPModel flipped (emissionsStart, emissionsEnd) edgeFlankSize ref sites
      matSeq = buildMatSeq model

      --ixs = fullSNPModelIxs flipped (emissionsStart, emissionsEnd) edgeFlankSize ref sites
      ixs = fullSNPModelIxs' flipped (V.map snd . stateLabels $ matSeq)

      x = 1
      emissions'' = V.map (normalize . regularize x) emissions'


  densities <- runHMMSum' ixs minionIndexMap' emissions'' matSeq
  let probs = map head $ map (\v -> map (/ sum v) v) densities
      outputStr = unlines . map show $ probs

  putStrLn outputStr
  writeFile outputFile outputStr
-}
