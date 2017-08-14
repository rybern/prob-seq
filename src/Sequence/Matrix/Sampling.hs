module Sequence.Matrix.Sampling where

import qualified Math.LinearAlgebra.Sparse as M
import qualified Data.Vector as V
import Sequence.Matrix.Operations
import Sequence.Matrix.Types
import Sequence.Matrix.Emissions
import Control.Monad.Random
import Control.Monad.Loops

-- NEED TO ADD SAMPLE UNIFORM

randToIO :: Rand StdGen a -> IO a
randToIO r = evalRand r <$> newStdGen

uniformSampleElemFrom :: (MonadRandom m) => [a] -> m a
uniformSampleElemFrom = fromList . toUniform
  where toUniform xs =
          let uniform = 1 / (fromIntegral (length xs))
          in map (\a -> (a, uniform)) xs

sampleSeqWithProb :: (MonadRandom m, Eq s) => (M.SparseVector Prob -> m Int) -> MatSeq s -> m ((V.Vector s, Int), Prob)
sampleSeqWithProb sample seq = do
  sample <- sampleSeq sample seq
  return $ (sample, stateSequenceProbability sample seq)

sampleSeq :: (MonadRandom m) => (M.SparseVector Prob -> m Int) -> MatSeq s -> m (V.Vector s, Int)
sampleSeq sample seq = do
  let trans = getTransWithEnds seq
  ixs <- sampleTrans sample trans
  let stateIxs = V.filter (\ix -> ix /= 1 && ix /= V.last ixs) ixs
      endIx = V.last ixs - M.height trans
  return (V.map ((stateLabels seq V.!) . (\x -> x - 2)) stateIxs, endIx)

-- this hangs on (ds [a])
-- The problem is the form of trans, 1 is looping to itself because there's no first column

sampleTrans :: (MonadRandom m) => (M.SparseVector Prob -> m Int) -> Trans -> m (V.Vector Int)
sampleTrans sample m = V.fromList . reverse <$> iterateUntilM
  (\path@(ix:_   ) -> ix > nStates m && length path > 1)
  (\(ix:rest) -> do
      ix' <- stepSequence sample m ix
      return $ ix':ix:rest)
  [1]

stepSequence :: (MonadRandom m) => (M.SparseVector Prob -> m Int) -> Trans -> Int -> m Int
stepSequence sample m ix = sample $ M.row m ix

vecUniformDist :: (MonadRandom m) => M.SparseVector Prob -> m Int
vecUniformDist v = fromList . map (\(ix, p) -> (ix, uniform)) $ assocs
  where assocs = tail . M.vecToAssocList $ v
        uniform = recip . fromIntegral . length $ assocs

vecDist :: (MonadRandom m) => M.SparseVector Prob -> m Int
vecDist = fromList . map (\(ix, p) -> (ix, toRational p)) . tail . M.vecToAssocList
