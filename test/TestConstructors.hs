{-# LANGUAGE OverloadedLists #-}
module TestConstructors where

import Data.Word
import Data.Monoid
import Data.List (find)
import Control.Monad
import qualified Data.Vector as V

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import Test.Tasty.HUnit

import Sequence

import ArbitraryConstructors

operationTests = testGroup "Operations"
  [
    seriesDistributesPropTest
  , deterministicIsConstantPropTest
  , localOption (QuickCheckTests 10000) exactSequenceProbabilityTest
  --, statisticalSequenceProbabilityTest
  ]

exactSequenceProbabilityTest = flip testProperty exactSequenceProbability $
  "Random seq constructor and exact sample probability match trans matrix sample probability"
exactSequenceProbability :: SmallProbSeq Word8 -> Property
exactSequenceProbability (SmallProbSeq (ast, seq)) =
  sequenceProbAST seq ast === stateSequenceProbability seq (buildMatSeq ast)

statisticalSequenceProbabilityTest = flip testProperty statisticalSequenceProbability $
  "Random seq constructor and exact sample probability match trans matrix sample probability"
statisticalSequenceProbability :: LargeProbSeq Word8 -> Property
statisticalSequenceProbability (LargeProbSeq (ast, seq)) = ioProperty $ do
  -- do statistics on these numbers
  -- sufficient n depends on the valid proportion, which is estimated over time
  -- so need some simple streaming. easy after defining sufficientN :: Prob -> Prob -> (Int, Eps)
  let n = 10000
  astPaths <- replicateM n $ sampleAST ast
  let matSeq = buildMatSeq ast
  matPaths <- replicateM n $ sampleSeq' matSeq

  let pathProportion = (/ fromIntegral n) . fromIntegral . length . filter (== seq)
      astProportion = pathProportion astPaths
      matProportion = pathProportion matPaths
      err = abs (astProportion - matProportion)

  return $ err > 0.001

aproxEq :: Double -> Double -> Bool
aproxEq a b = (a - eps) < b && b < (a + eps)
  where eps = 0.00000001

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

{-
findFailing :: IO [String]
findFailing = do
  result <- quickCheckResult correctSampleProbProp
  case result of
    failure@(Failure {}) -> return $ failingTestCase failure
    _ -> findFailing
-}

{-
--sampleStateSequenceProbability :: SequenceConstructor s -> (Gen (V.Vector s, Prob), Prob)
testSequence :: (Eq s) => ProbSeq s -> IO (V.Vector s, Bool)
testSequence consts = generate $ do
  (path, p) <- fst $ sampleStateSequenceProbability consts
  let works = any (== p) $ stateSequenceProbability (buildSequence consts) path
  return (path, works)

testSequenceUntil :: (Eq s) => ProbSeq s -> IO (Maybe (V.Vector s, Bool))
testSequenceUntil = let findFail = find (not . snd) . take 100000
                    in (findFail <$>) . sequence . repeat . testSequence
-}
