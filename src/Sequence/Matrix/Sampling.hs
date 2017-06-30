module Sequence.Matrix.Sampling where

import qualified Math.LinearAlgebra.Sparse as M
import qualified Data.Vector as V
import Sequence.Matrix.Operations (getTrans)
import Sequence.Matrix.Types
import Control.Monad.Random
import Control.Monad.Loops

sampleSeq :: MatSeq s -> IO (V.Vector s)
sampleSeq = randToIO . sampleSeq'

randToIO :: Rand StdGen a -> IO a
randToIO r = evalRand r <$> newStdGen

sampleSeq' :: (MonadRandom m) => MatSeq s -> m (V.Vector s)
sampleSeq' seq = do
  ixs <- sampleTrans (getTrans seq)
  let nontokenIxs = V.filter (\ix -> ix /= 1 && ix /= V.last ixs) ixs
  return $ V.map ((stateLabels seq V.!) . (\x -> x - 2)) nontokenIxs

sampleTrans :: (MonadRandom m) => Trans -> m (V.Vector Int)
sampleTrans m = V.fromList . reverse <$> iterateUntilM
  (\(ix:_   ) -> ix == M.height m)
  (\(ix:rest) -> do
      ix' <- stepSequence m ix
      return $ ix':ix:rest)
  [1]

stepSequence :: (MonadRandom m) => Trans -> Int -> m Int
stepSequence m ix = vecToRandDist $ M.row m ix

vecToRandDist :: (MonadRandom m) => M.SparseVector Prob -> m Int
vecToRandDist = fromList . map (\(ix, p) -> (ix, toRational p)) . M.vecToAssocList
