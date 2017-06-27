{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module SequenceConstructor where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Sequence
import Control.Monad
import Data.Monoid ((<>))
import qualified Data.Vector as V
import qualified Math.LinearAlgebra.Sparse as M

data SequenceConstructor s = EmptySequence
                           | DeterministicSequence (V.Vector s)
                           | EitherOr Prob (SequenceConstructor s) (SequenceConstructor s)
                           | AndThen (SequenceConstructor s) (SequenceConstructor s)
                           -- will probably end up using [s] and concat to make types work
                           | Collapse Int (SequenceConstructor s)
                           | Possibly Prob (SequenceConstructor s)
                           | UniformDistOver [SequenceConstructor s]
                           | FiniteDistOver [(SequenceConstructor s, Prob)]
                           | SkipDist Dist (SequenceConstructor s)
                           | GeometricRepeat Prob (SequenceConstructor s)
                           | FiniteDistRepeat [Prob] (SequenceConstructor s)
                           | UniformDistRepeat Int (SequenceConstructor s)
                           | ReverseSequence (SequenceConstructor s)
                           deriving (Show)

-- don't support geometric or collapse yet
buildSequence :: SequenceConstructor s -> Sequence s
buildSequence EmptySequence = emptySequence
buildSequence (DeterministicSequence v) = deterministicSequence v
buildSequence (EitherOr p constA constB) = eitherOr p (buildSequence constA) (buildSequence constB)
buildSequence (AndThen constA constB) = andThen (buildSequence constA) (buildSequence constB)
buildSequence (Possibly p const) = possibly p (buildSequence const)
buildSequence (UniformDistOver consts) = uniformDistOver (map buildSequence consts)
buildSequence (FiniteDistOver constPairs) = finiteDistOver [(buildSequence const, p) | (const, p) <- constPairs]
buildSequence (SkipDist dist const) = skipDist dist (buildSequence const)
buildSequence (FiniteDistRepeat ps const) = finiteDistRepeat ps (buildSequence const)
buildSequence (UniformDistRepeat n const) = uniformDistRepeat n (buildSequence const)
buildSequence (ReverseSequence const) = reverseSequence (buildSequence const)

-- if you are about to return an empty sequence, return it with probability p0
fixEmptySequenceProb :: Prob -> (V.Vector s, Prob) -> (V.Vector s, Prob)
fixEmptySequenceProb p0 (path, prob)
  | V.null path = (path, p0)
  | otherwise = (path, prob)

-- Need to keep an aggregate probability of empty sequence.
-- That could be done with a pre-traversal step?
sampleStateSequenceProbability :: SequenceConstructor s -> (Gen (V.Vector s, Prob), Prob)
sampleStateSequenceProbability EmptySequence = (return (V.empty, 1.0), 1.0)
sampleStateSequenceProbability (DeterministicSequence v) =
  if V.null v
  then sampleStateSequenceProbability EmptySequence
  else (return (v, 1.0), 0.0)
sampleStateSequenceProbability (EitherOr p constA constB) =
  let (sampleA, p0A) = sampleStateSequenceProbability constA
      (sampleB, p0B) = sampleStateSequenceProbability constB
      p0' = p * p0A + (1 - p) * p0B
      fixEmpty = fixEmptySequenceProb p0'
  in (, p0') $ do
    (pathA, probA) <- sampleA
    (pathB, probB) <- sampleB
    elements [ fixEmpty (pathA, probA * p)
             , fixEmpty (pathB, probB * (1 - p)) ]
sampleStateSequenceProbability (AndThen constA constB) =
  let (sampleA, p0A) = sampleStateSequenceProbability constA
      (sampleB, p0B) = sampleStateSequenceProbability constB
      p0' = p0A * p0B
      fixEmpty = fixEmptySequenceProb p0'
  in (, p0') $ do
    (pathA, probA) <- sampleA
    (pathB, probB) <- sampleB
    return $ fixEmpty (pathA <> pathB, probA * probB)
--sampleStateSequenceProbability (Possibly _ EmptySequence) =
  --sampleStateSequenceProbability EmptySequence
sampleStateSequenceProbability (Possibly p const) =
  let (sample, p0) = sampleStateSequenceProbability const
      p0' = 1 - p + p * p0
      fixEmpty = fixEmptySequenceProb p0'
  in (, p0') $ do
    (path, prob) <- sample
    elements [ fixEmpty (path, p * prob)
             , fixEmpty (V.empty, 1 - p)]
sampleStateSequenceProbability (UniformDistOver consts) =
  let n = length consts
  in sampleStateSequenceProbability (FiniteDistOver (zip
                                                      consts
                                                      (replicate n (1 / fromIntegral n))))
sampleStateSequenceProbability (FiniteDistOver constPairs) =
  let (samples, p0s) = unzip . flip map constPairs $ \(c, p) ->
        let (sample, p0) = sampleStateSequenceProbability c
        in ((sample, p), p0 * p)
      p0' = sum p0s
      fixEmpty = fixEmptySequenceProb p0'
  in (, p0') $ do
    (sample, branchProb) <- elements samples
    (path, prob) <- sample
    return $ fixEmpty (path, prob * branchProb)
--sampleStateSequenceProbability (FiniteDistRepeat _ EmptySequence) =
  --sampleStateSequenceProbability EmptySequence
sampleStateSequenceProbability (FiniteDistRepeat ps const) =
  let (sample, p0) = sampleStateSequenceProbability const
      ps' = 1-sum ps : ps
      p0' = sum . zipWith (*) ps' . map (p0^) $ [0..]
      fixEmpty = fixEmptySequenceProb p0'
  in (, p0') $ do
    nNonempty <- choice (0, length ps)

    let geoms = map (\n -> (1 - p0) * p0 ^ n) [0..]
        branchProbs' = 


-- NOTE:
          -- is it possible to first use the AST to sample a sequence, then to enumerate its paths?
          -- then could sum together the AST paths and the matrix paths
          -- this way becomes quite painful
          
    -- do
    -- (repeatNum, branchProb) <- elements (zip [0..] ps')
    -- (paths, probs) <- unzip <$> replicateM repeatNum sample
    -- return $ fixEmpty (mconcat paths, product probs * branchProb)
  -- this is incorrect because the matrix doesn't differentiate between the sources of replication
  -- e.g. there is no difference between branch 5, first is empty and branch 4, first is nonempty. they are grouped.

sampleStateSequenceProbability (UniformDistRepeat n const) =
  sampleStateSequenceProbability (FiniteDistRepeat (replicate n (1 / fromIntegral n)) const)
--sampleStateSequenceProbability (ReverseSequence EmptySequence) =
  --sampleStateSequenceProbability EmptySequence
sampleStateSequenceProbability (ReverseSequence const) =
  let (sample, p0) = sampleStateSequenceProbability const
      p0' = p0
      fixEmpty = fixEmptySequenceProb p0'
  in (, p0') $ do
    (path, prob) <- sample
    return $ fixEmpty (V.reverse path, prob)
{-
not supporting skipdist testing yet
sampleStateSequenceProbability (SkipDist dist const) =
  let (sample, p0) = sampleStateSequenceProbability const
  in (, p0) $ do
    (path, prob) <- sampleStateSequenceProbability const
    return $ skipdistPathSample (M.fillVec dist) (path, prob)

-- just write recursively
skipdistPathSample :: [Prob] -> (V.Vector s, Prob) -> (V.Vector s, Prob)
skipdistPathSample dist (path, prob) = undefined
-}

probGen :: Gen Prob
probGen = choose (0, 1)

distGen :: Int -> Gen [Prob]
distGen n = do
  is <- replicateM n probGen
  return $ (/ sum is) <$> is

geometricArbitraryConstructor :: (Arbitrary s) => Prob -> Gen (SequenceConstructor s)
geometricArbitraryConstructor p = frequency [ (round (p * 1000),       arbitraryBranchConstructor p')
                                            , (round ((1 - p) * 1000), arbitraryLeafConstructor)]
  where p' = 0.95 * p

arbitraryLeafConstructor :: (Arbitrary s) => Gen (SequenceConstructor s)
arbitraryLeafConstructor = oneof
  [
    return $ EmptySequence
  , do -- n <- choose (0, 10)
       let n = 1
       v <- V.replicateM n arbitrary
       return (DeterministicSequence v)
  ]

arbitraryEitherOr ::
  Arbitrary s => Prob -> Gen (SequenceConstructor s)
arbitraryEitherOr p = do
  constA <- geometricArbitraryConstructor p
  constB <- geometricArbitraryConstructor p
  p <- probGen
  return (EitherOr p constA constB)

arbitraryAndThen ::
  Arbitrary s => Prob -> Gen (SequenceConstructor s)
arbitraryAndThen p = do
  constA <- geometricArbitraryConstructor p
  constB <- geometricArbitraryConstructor p
  return (AndThen constA constB)

arbitraryPossibly ::
  Arbitrary s => Prob -> Gen (SequenceConstructor s)
arbitraryPossibly p = do
  const <- geometricArbitraryConstructor p
  p <- probGen
  return (Possibly p const)

arbitraryUniformDistOver ::
  Arbitrary s => Prob -> Gen (SequenceConstructor s)
arbitraryUniformDistOver p = do
  n <- choose (1, 10)
  consts <- replicateM n (geometricArbitraryConstructor (p / fromIntegral n))
  return (UniformDistOver consts)

arbitraryFiniteDistOver ::
  Arbitrary s => Prob -> Gen (SequenceConstructor s)
arbitraryFiniteDistOver p = do
  n <- choose (1, 5)
  consts <- replicateM n (geometricArbitraryConstructor (p / fromIntegral n))
  ps <- distGen n
  let constPairs = zip consts ps
  return (FiniteDistOver constPairs)

arbitraryFiniteDistRepeat ::
  Arbitrary s => Prob -> Gen (SequenceConstructor s)
arbitraryFiniteDistRepeat p = do
  const <- geometricArbitraryConstructor p
  n <- choose (1, 4)
  (_:ps) <- distGen n
  return (FiniteDistRepeat ps const)

arbitraryUniformDistRepeat ::
  Arbitrary s => Prob -> Gen (SequenceConstructor s)
arbitraryUniformDistRepeat p = do
  const <- geometricArbitraryConstructor p
  n <- choose (1, 10)
  return (UniformDistRepeat n const)

arbitraryReverseSequence ::
  Arbitrary s => Prob -> Gen (SequenceConstructor s)
arbitraryReverseSequence p = do
  const <- geometricArbitraryConstructor p
  return (ReverseSequence const)

{-
arbitrarySkipDist ::
  Arbitrary s => Prob -> Gen (SequenceConstructor s)
arbitrarySkipDist = do
  const <- arbitrary
  n <- choose (0, 10)
  ps <- distGen n
  let dist = M.vecFromAssocList (zip [1..] ps)
  return (SkipDist dist const)
-}

arbitraryBranchConstructor :: (Arbitrary s) => Prob -> Gen (SequenceConstructor s)
arbitraryBranchConstructor p = oneof . map ($ p) $
  [
    arbitraryEitherOr
  , arbitraryAndThen
  --, arbitraryPossibly
  --, arbitraryUniformDistOver
  --, arbitraryFiniteDistOver
  , arbitraryFiniteDistRepeat
  --, arbitraryUniformDistRepeat

    -- reverse sequence is broken. test case:
      -- (ReverseSequence (Possibly 0.15 (DeterministicSequence (V.singleton 1))))
  --, arbitraryReverseSequence
  ]

instance (Arbitrary s) => Arbitrary (SequenceConstructor s) where
  arbitrary = geometricArbitraryConstructor 0.2

newtype SampledSequenceConstructor s = SampledSequenceConstructor (SequenceConstructor s, (V.Vector s, Prob))
  deriving (Show)

instance (Arbitrary s) => Arbitrary (SampledSequenceConstructor s) where
  arbitrary = do
    const <- arbitrary
    path <- fst $ sampleStateSequenceProbability const
    return $ SampledSequenceConstructor (const, path)
