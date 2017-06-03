module Sampling where

import qualified Math.LinearAlgebra.Sparse as M
import Types
import qualified Data.Vector as V
import Control.Monad.Random
import System.Random
import Control.Monad.Loops

sample :: Sequence s -> IO (V.Vector s)
sample = randToIO . sample'

type Rand' = Rand StdGen
randToIO :: Rand' a -> IO a
randToIO r = evalRand r <$> newStdGen

sample' :: Sequence s -> Rand' (V.Vector s)
sample' seq = do
  ixs <- sampleTrans (transMatrix seq)
  let nontokenIxs = V.filter (\ix -> ix /= 1 && ix /= V.last ixs) ixs
  return $ V.map ((stateIxs seq V.!) . (\x -> x - 2)) nontokenIxs

sampleTrans :: Trans -> Rand' (V.Vector Int)
sampleTrans m = V.fromList . reverse <$> iterateUntilM
  (\(ix:_   ) -> ix == M.height m)
  (\(ix:rest) -> do
      ix' <- stepSequence m ix
      return $ ix':ix:rest)
  [1]

stepSequence :: Trans -> Int -> Rand' Int
stepSequence m ix = vecToRandDist $ M.row m ix

vecToRandDist :: M.SparseVector Prob -> Rand' Int
vecToRandDist = fromList . map (\(ix, p) -> (ix, toRational p)) . M.vecToAssocList

