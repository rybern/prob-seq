module ArbitraryAST where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Control.Monad
import Control.Monad.Random
import Data.Monoid ((<>))
import qualified Data.Vector as V
import qualified Math.LinearAlgebra.Sparse as M

import Sequence.AST
import Sequence.Types
import Utils

newtype SmallProbSeq s = SmallProbSeq (ProbSeq s, V.Vector s) deriving Show
instance (Arbitrary s) => Arbitrary (SmallProbSeq s) where
  arbitrary = do
    constr <- geometricArbitraryConstructor 0.35
    path <- sampleASTUniform constr
    return $ SmallProbSeq (constr, path)

newtype LargeProbSeq s = LargeProbSeq (ProbSeq s, V.Vector s) deriving Show
instance (Arbitrary s) => Arbitrary (LargeProbSeq s) where
  arbitrary = do
    constr <- geometricArbitraryConstructor 0.99
    path <- sampleASTUniform constr
    return $ LargeProbSeq (constr, path)

geometricArbitraryConstructor :: (Arbitrary s)=> Prob -> Gen (ProbSeq s)
geometricArbitraryConstructor p = frequency [ (round (p * 1000),       arbitraryBranchConstructor p')
                                            , (round ((1 - p) * 1000), arbitraryLeafConstructor)]
  where p' = 0.95 * p

arbitraryBranchConstructor :: (Arbitrary s) => Prob -> Gen (ProbSeq s)
arbitraryBranchConstructor p = oneof . map ($ p) $
  [
    arbitraryEitherOr
  , arbitraryAndThen
  , arbitraryPossibly
  , arbitraryUniformDistOver
  , arbitraryFiniteDistOver
  , arbitraryFiniteDistRepeat
  , arbitraryUniformDistRepeat

    -- reverse sequence is broken. test case:
      -- (ReverseSequence (Possibly 0.15 (DeterministicSequence (V.singleton 1))))
  --, arbitraryReverseSequence
  ]

arbitraryLeafConstructor :: (Arbitrary s) => Gen (ProbSeq s)
arbitraryLeafConstructor = oneof
  [
    return $ EmptySequence
  , do -- n <- choose (0, 10)
       let n = 1
       v <- V.replicateM n arbitrary
       return (DeterministicSequence v)
  ]

probGen :: Gen Prob
probGen = toRational <$> (choose (0, 1) :: Gen Double)

distGen :: Int -> Gen [Prob]
distGen n = do
  is <- replicateM n probGen
  return $ (/ sum is) <$> is

arbitraryEitherOr ::
  Arbitrary s => Prob -> Gen (ProbSeq s)
arbitraryEitherOr p = do
  constA <- geometricArbitraryConstructor p
  constB <- geometricArbitraryConstructor p
  p <- probGen
  return (EitherOr p constA constB)

arbitraryAndThen ::
  Arbitrary s => Prob -> Gen (ProbSeq s)
arbitraryAndThen p = do
  constA <- geometricArbitraryConstructor p
  constB <- geometricArbitraryConstructor p
  return (AndThen constA constB)

arbitraryPossibly ::
  Arbitrary s => Prob -> Gen (ProbSeq s)
arbitraryPossibly p = do
  const <- geometricArbitraryConstructor p
  p <- probGen
  return (Possibly p const)

arbitraryUniformDistOver ::
  Arbitrary s => Prob -> Gen (ProbSeq s)
arbitraryUniformDistOver p = do
  n <- choose (1, 5)
  consts <- replicateM n (geometricArbitraryConstructor (p / fromIntegral n))
  return (UniformDistOver consts)

arbitraryFiniteDistOver ::
  Arbitrary s => Prob -> Gen (ProbSeq s)
arbitraryFiniteDistOver p = do
  n <- choose (1, 5)
  consts <- replicateM n (geometricArbitraryConstructor (p / fromIntegral n))
  ps <- distGen n
  let constPairs = zip consts ps
  return (FiniteDistOver constPairs)

arbitraryFiniteDistRepeat ::
  Arbitrary s => Prob -> Gen (ProbSeq s)
arbitraryFiniteDistRepeat p = do
  const <- geometricArbitraryConstructor p
  n <- choose (1, 4)
  (_:ps) <- distGen n
  return (FiniteDistRepeat ps const)

arbitraryUniformDistRepeat ::
  Arbitrary s => Prob -> Gen (ProbSeq s)
arbitraryUniformDistRepeat p = do
  const <- geometricArbitraryConstructor p
  n <- choose (1, 4)
  return (UniformDistRepeat n const)

arbitraryReverseSequence ::
  Arbitrary s => Prob -> Gen (ProbSeq s)
arbitraryReverseSequence p = do
  const <- geometricArbitraryConstructor p
  return (ReverseSequence const)

{-
arbitrarySkipDist ::
  Arbitrary s => Prob -> Gen (ProbSeq s)
arbitrarySkipDist = do
  const <- arbitrary
  n <- choose (0, 10)
  ps <- distGen n
  let dist = M.vecFromAssocList (zip [1..] ps)
  return (SkipDist dist const)
-}

{-
newtype SampledProbSeq s = SampledProbSeq (ProbSeq s, V.Vector s, Prob))
  deriving (Show)

instance (Arbitrary s) => Arbitrary (SampledProbSeq s) where
  arbitrary = do
    const <- arbitrary
    path <- sampleASTUniform const
    return $ SampledProbSeq (const, path)
-}
