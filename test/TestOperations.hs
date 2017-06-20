{-# LANGUAGE OverloadedLists #-}
module TestOperations where

import Data.Word
import Data.Monoid
import Control.Monad
import qualified Data.Vector as V

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Sequence
import Sequence.Sampling
import Sequence.Emission
import SequenceConstructor

operationTests = testGroup "Operations"
  [
    seriesDistributesPropTest
  , deterministicIsConstantPropTest
  , correctSampleProbPropTest
  ]

correctSampleProbPropTest = testProperty "Random seq constructor and exact sample probability match trans matrix sample probability" correctSampleProbProp
correctSampleProbProp :: SampledSequenceConstructor Word8 -> Bool
correctSampleProbProp (SampledSequenceConstructor (consts, (path, p))) =
  any (== p) $ stateSequenceProbability (buildSequence consts) path

subsetsProd :: (Num a) => V.Vector a -> V.Vector a
subsetsProd = V.map getProduct . subsets . V.map Product

subsets :: (Monoid m) => V.Vector m -> V.Vector m
subsets m = if V.length m <= 1
            then m
            else let first = V.head m
                     withRest = subsets $ V.tail m
                 in first `V.cons` (withRest <> V.map (first <>) withRest)

seriesDistributesPropTest = testProperty "andThen distributes over <>" seriesDistributesProp
seriesDistributesProp :: V.Vector Word8 -> V.Vector Word8 -> Bool
seriesDistributesProp s1 s2 =
  deterministicSequence s1 <> deterministicSequence s2 == deterministicSequence (s1 <> s2)

deterministicIsConstantPropTest = testProperty "deterministicSequence . sampleSeq == id" $
  \v -> ioProperty $ deterministicIsConstantProp v
deterministicIsConstantProp :: V.Vector Word8 -> IO Bool
deterministicIsConstantProp s1 = do
  let seq = deterministicSequence s1
  samples <- replicateM 10 $ sampleSeq seq
  return $ all (== s1) samples

instance Arbitrary e => Arbitrary (V.Vector e) where
  arbitrary = V.fromList <$> arbitrary
