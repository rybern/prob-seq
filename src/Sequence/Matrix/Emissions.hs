{-# LANGUAGE OverloadedLists #-}
module Sequence.Matrix.Emissions
  (
    stateSequenceProbability
  ) where

import Sequence.Matrix.Types
import Sequence.Matrix.Operations (getTrans)
import Control.Monad
import qualified Data.Vector as V
import qualified Math.LinearAlgebra.Sparse as M

stateSequenceProbability :: (Eq s) => V.Vector s -> MatSeq s -> Prob
stateSequenceProbability path seq = sum . pathProbs (getTrans seq) . stateSequenceIxs seq $ path

stateSequenceIxs :: (Eq s) => MatSeq s -> V.Vector s -> [V.Vector M.Index]
stateSequenceIxs seq = V.toList . addEnd . V.map (stateIxs seq)
  where endIx = V.length (stateLabels seq) + 2
        addEnd = (`V.snoc` [endIx])

stateIxs :: (Eq s) => MatSeq s -> s -> V.Vector M.Index
stateIxs (MatSeq {stateLabels = stateLabels}) label =
  flip V.imapMaybe stateLabels $ \ix label' ->
                                   if label' == label
                                   then Just (ix + 2)
                                   else Nothing

pathProbs :: Trans -> [V.Vector M.Index] -> V.Vector Prob
pathProbs = pathProbs' 1

pathProbs' :: M.Index -> Trans -> [V.Vector M.Index] -> V.Vector Prob
pathProbs' _ _ [] = [1.0]
pathProbs' current trans (nexts:rest) = do
  next <- nexts

  let transProb = trans M.# (current, next)
  guard $ transProb > 0

  nextPathProb <- pathProbs' next trans rest

  return $ transProb * nextPathProb
