{-# LANGUAGE OverloadedLists, TupleSections, RecordWildCards #-}
module ArbitraryConstructors where

import Control.Monad.Random
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Fix
import Data.Monoid
import Data.Maybe
import Data.Foldable

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

import Sequence.Matrix
import Sequence.Matrix.Types
import Sequence.Matrix.Sampling
import Sequence.Matrix.Emissions
import Sequence.Matrix.Operations
import Sequence.Constructors

  -- reverse sequence is broken. test case:
  -- (ReverseSequence (Possibly 0.15 (DeterministicSequence (V.singleton 1))))

arbitraryProbSeq :: Arbitrary s => Prob -> Gen (ProbSeq s)
arbitraryProbSeq = anaM geometricArbitraryConstructor

geometricArbitraryConstructor :: (Arbitrary s) => Prob -> Gen (Constructor s Prob)
geometricArbitraryConstructor p = (const (0.60 * p) <$>) <$>
  frequency [ (round (p * 1000),       arbitraryBranchConstructor)
            , (round ((1 - p) * 1000), arbitraryLeafConstructor)]

arbitraryBranchConstructor :: (Arbitrary s) => Gen (Constructor s ())
arbitraryBranchConstructor = oneof $
  [
    arbitraryEitherOr
  , arbitraryAndThen
  --, arbitraryPossibly
  --, arbitraryUniformDistOver
  --, arbitraryFiniteDistOver
  --, arbitraryFiniteDistRepeat
  --, arbitraryUniformDistRepeat
  --, arbitraryReverseSequence
  ]

arbitraryLeafConstructor :: (Arbitrary s) => Gen (Constructor s ())
arbitraryLeafConstructor = oneof
  [
    return $ EmptySequence
  , arbitraryDeterministicSequence
  , arbitrarySkip
  ]

arbitrarySkip :: Arbitrary s => Gen (Constructor s ())
arbitrarySkip = do
  n <- choose (0, 3)
  return (Skip n)

arbitraryDeterministicSequence :: Arbitrary s => Gen (Constructor s ())
arbitraryDeterministicSequence = do
  --let n = 1
  n <- choose (0, 4)
  v <- V.replicateM n arbitrary
  return (DeterministicSequence v)

probGen :: Gen Prob
probGen = toRational <$> (choose (0, 1) :: Gen Double)

distGen :: Int -> Gen [Prob]
distGen n = do
  is <- replicateM n probGen
  return $ (/ sum is) <$> is

arbitraryEitherOr ::
  Arbitrary s => Gen (Constructor s ())
arbitraryEitherOr = do
  q <- probGen
  return (EitherOr q () ())

arbitraryAndThen ::
  Arbitrary s => Gen (Constructor s ())
arbitraryAndThen = do
  return (AndThen () ())

arbitraryPossibly ::
  Arbitrary s => Gen (Constructor s ())
arbitraryPossibly = do
  q <- probGen
  return (Possibly q ())

arbitraryUniformDistOver ::
  Arbitrary s => Gen (Constructor s ())
arbitraryUniformDistOver = do
  n <- choose (1, 5)
  return (UniformDistOver $ replicate n ())

arbitraryFiniteDistOver ::
  Arbitrary s => Gen (Constructor s ())
arbitraryFiniteDistOver = do
  n <- choose (1, 5)
  ps <- distGen n
  let constPairs = map ((), ) ps
  return (FiniteDistOver constPairs)

arbitraryFiniteDistRepeat ::
  Arbitrary s => Gen (Constructor s ())
arbitraryFiniteDistRepeat = do
  n <- choose (1, 4)
  (_:ps) <- distGen n
  return (FiniteDistRepeat ps ())

arbitraryUniformDistRepeat ::
  Arbitrary s => Gen (Constructor s ())
arbitraryUniformDistRepeat = do
  n <- choose (1, 4)
  return (UniformDistRepeat n ())

arbitraryReverseSequence ::
  Arbitrary s => Gen (Constructor s ())
arbitraryReverseSequence = do
  return (ReverseSequence ())
