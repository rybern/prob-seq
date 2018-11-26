{-# LANGUAGE RecordWildCards #-}
module SMoL.Inference where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.IO.Unsafe

--import SMoL
import SMoL.Types
import SMoL.Matrix
import SMoL.Matrix.ProbSeqMatrixUtils
import SparseMatrix hiding (trans)

type VecMat a = Vector (Vector a) -- use Data.Matrix?
data Emissions a d = Emissions {
    emissionMat :: VecMat a
  , indexMap :: Map d Int
  } deriving Show

mapEmissions :: (VecMat a -> VecMat a) -> Emissions a d -> Emissions a d
mapEmissions f ems = ems { emissionMat = f (emissionMat ems) }

type InferenceEngine a res = Int -> [(Int, Int, Double)] -> VecMat a -> Vector Int -> res

infer :: (Ord a, Show a)
      => InferenceEngine b res
      -> Emissions b a
      -> MatSeq a
      -> res
infer fn (Emissions {..}) priorSeq = fn ns triples emissionMat permutation
  where permutation = buildEmissionPerm indexMap priorSeq
        triples = matSeqTriples priorSeq
        ns = (nStates (trans priorSeq))

inferUnsafe :: (Ord a, Show a)
            => InferenceEngine b (IO res)
            -> Emissions b a
            -> MatSeq a
            -> res
inferUnsafe fn emissions priorSeq = unsafePerformIO $ infer fn emissions priorSeq

matSeqTriples :: MatSeq a
              -> [(Int, Int, Double)]
matSeqTriples = map (\((r, c), p) -> (r - 1, c - 1, p)) . tail . toAssocList . cleanTrans . trans

buildEmissionPerm :: (Ord a, Show a) => Map a Int -> MatSeq a -> V.Vector Int
buildEmissionPerm m = V.map (getIndex . stateLabel) . stateLabels
  where getIndex k = case Map.lookup k m of
          Just ix -> ix
          Nothing ->
            error $ "There was a label in the model that was not in the emissions data: " ++ show k
