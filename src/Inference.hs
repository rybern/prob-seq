{-# LANGUAGE RecordWildCards #-}
module Inference where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.IO.Unsafe

--import Sequence
import Sequence.Types
import Sequence.Matrix
import Sequence.Matrix.ProbSeqMatrixUtils
import SparseMatrix hiding (trans)

type VecMat = Vector (Vector Prob) -- use Data.Matrix?
data Emissions d = Emissions {
    emissions :: VecMat
  , indexMap :: Map d Int
  } deriving Show

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
buildEmissionPerm m = V.map (getIndex . stateLabel) . stateLabels
  where getIndex k = case Map.lookup k m of
          Just ix -> ix
          Nothing ->
            error $ "There was a label in the model that was not in the emissions data: " ++ show k
