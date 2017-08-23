{-# LANGUAGE RankNTypes, OverloadedLists, TupleSections, RecordWildCards #-}
module ConstructorSampling where

import Control.Monad.Random
import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.List
import Data.Monoid
import Data.Function

import Sequence.Matrix
import Sequence.Matrix.Types
import Sequence.Matrix.Sampling
import Sequence.Matrix.Emissions
import Sequence.Matrix.Operations
import Sequence.Constructors

sampleConstructor :: Eq s
                  => Constructor s (MatSeq s)
                  -> (Vector s, Int)
                  -> Prob
sampleConstructor constructor s = constructorSeqProb (fmap sequenceDist constructor) s

data SequenceDist s = SeqDist {
    seqProb :: (Vector s, Int) -> Prob
  , possibleSkips :: [Int]
  , seqSuffixProb :: Int -> (Vector s, Int) -> Prob
  }

sequenceDist :: (Eq s)
             => MatSeq s
             -> SequenceDist s
sequenceDist ms = SeqDist {
    seqProb = \s -> stateSequenceProbability s ms
  , possibleSkips = reachableSkips (trans ms)
  , seqSuffixProb = \i s -> sequenceSuffixProbability i s ms
  }

constructorSeqProb :: (Eq s)
                  => Constructor s (SequenceDist s)
                  -> (Vector s, Int)
                  -> Prob
constructorSeqProb EmptySequence s = if s == (V.empty, 0) then 1 else 0
constructorSeqProb (DeterministicSequence v) s = if s == (v, 0) then 1 else 0
constructorSeqProb (Skip n) s = if s == (V.empty, n) then 1 else 0
constructorSeqProb (EitherOr p m1 m2) s = p * seqProb m1 s + (1 - p) * seqProb m2 s
constructorSeqProb (AndThen m1 m2) (seq, sk) = sum $ do
  (left, right) <- map (\n -> V.splitAt n seq) [0..V.length seq]
  skipLeft <- possibleSkips m1
  let pLeft = seqProb m1 (left, skipLeft)
  guard $ pLeft > 0
  let pRight = seqSuffixProb m2 skipLeft (right, sk)
  guard $ pRight > 0
  return $ pRight * pLeft
constructorSeqProb (Possibly p m) s = if s == (V.empty, 0)
                                     then (1 - p) + p * seqProb m s
                                     else p * seqProb m s
constructorSeqProb (FiniteDistOver ms) s = sum $ map (\(m, p) -> p * seqProb m s) ms
--constructorSeqProb (GeometricRepeat p m) s = undefined

{-
sampleConstructor sampleList (FiniteDistRepeat ps m) = do
  (n, p) <- sampleList (zip [0..] ps)
  samples <- replicateM n (sampleSeq vecUniformDist m)

  let join (s1, sk1) (s2, sk2) =
        let overflow = s1 - V.length sk2
        in if overflow > 0
           then (s1, overflow + sk2)
           else (s1 <> V.drop sk1 s2, sk2)
      seq = foldl1' join samples

sampleConstructor sampleList (UniformDistRepeat n m) = undefined
sampleConstructor _ (ReverseSequence m) = do
  ((v, _), p) <- sampleSeqWithProb vecUniformDist m
  return (V.reverse v, p)
sampleConstructor sampleList (Collapse n m) = undefined
-}

-- sampleSeq hangs on sampling empty
