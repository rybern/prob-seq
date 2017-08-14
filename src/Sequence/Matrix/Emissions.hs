{-# LANGUAGE OverloadedLists #-}
module Sequence.Matrix.Emissions
  (
    stateSequenceProbability
  , sequenceSuffixProbability
  ) where

import Sequence.Matrix.Types
import Sequence.Matrix.Operations
import Control.Monad
import qualified Data.Vector as V
import qualified Math.LinearAlgebra.Sparse as M

sequenceSuffixProbability :: (Eq s) => Int -> (V.Vector s, Int) -> MatSeq s -> Prob
sequenceSuffixProbability skipped (seq, nSkip) m =
  stateSequenceProbability (seq, nSkip) $ skip skipped `andThen` m

stateSequenceProbability :: (Eq s) => (V.Vector s, Int) -> MatSeq s -> Prob
stateSequenceProbability (path, skip) seq = sum . pathProbs (getTransWithEnds seq) . stateSequenceIxs seq $ (path, skip)

stateSequenceIxs :: (Eq s) => MatSeq s -> (V.Vector s, Int) -> [V.Vector M.Index]
stateSequenceIxs seq (path, skip) = V.toList . addEnd . V.map (stateIxs seq) $ path
  where endIx = V.length (stateLabels seq) + 2
        addEnd = (`V.snoc` [endIx + skip])

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