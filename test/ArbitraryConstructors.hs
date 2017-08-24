{-# LANGUAGE TupleSections #-}
module ArbitraryConstructors where

import Control.Monad
import qualified Data.Vector as V
import Data.Fix

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

import Sequence.Matrix.Types
import Sequence.Constructors

-- We need tree-shaped state labels in order to not change type after collapsing
data Tree s = Branch (V.Vector (Tree s))
            | Leaf s
            deriving (Show, Eq)

newtype SmallProbSeq s = SmallProbSeq (ProbSeq (Tree s))
  deriving Show

treeToVec :: Tree s -> V.Vector (Tree s)
treeToVec (Branch v) = v
treeToVec (Leaf s) = V.singleton (Leaf s)

vecToTree :: V.Vector (Tree s) -> Tree s
vecToTree = Branch

instance Arbitrary s => Arbitrary (SmallProbSeq s) where
  arbitrary = SmallProbSeq <$> arbitraryProbSeq 0.9

instance Arbitrary s => Arbitrary (Tree s) where
  arbitrary = Leaf <$> arbitrary

arbitraryProbSeq :: Arbitrary s => Prob -> Gen (ProbSeq (Tree s))
arbitraryProbSeq = anaM geometricArbitraryConstructor

geometricArbitraryConstructor :: (Arbitrary s) => Prob -> Gen (Constructor (Tree s) Prob)
geometricArbitraryConstructor p = (const (0.60 * p) <$>) <$>
  frequency [ (round (p * 1000),       arbitraryBranchConstructor)
            , (round ((1 - p) * 1000), arbitraryLeafConstructor)]


arbitraryBranchConstructor :: (Arbitrary s) => Gen (Constructor (Tree s) ())
arbitraryBranchConstructor = oneof $
  [
    arbitraryEitherOr
  , arbitraryAndThen
  , arbitrarySkip
  , arbitraryPossibly
  , arbitraryFiniteDistOver
  , arbitraryReverseSequence
  --, arbitraryCollapse
  --, arbitraryUniformDistOver
  --, arbitraryFiniteDistRepeat
  --, arbitraryUniformDistRepeat
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

-- s needs to either be a fully recursive structure or a list with a delimiter and bookends, like (,,,,)
arbitraryCollapse ::
  Arbitrary s => Gen (Constructor (Tree s) ())
arbitraryCollapse = do
  n <- choose (1, 4)
  return (Collapse treeToVec vecToTree n ())

