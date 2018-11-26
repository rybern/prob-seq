{-# LANGUAGE TupleSections, FlexibleInstances #-}
module RandomSubmatrixTest where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

import SparseMatrix (SparseMatrix)
import Sequence.Matrix.SparseMatrixUtils
import Sequence.Matrix.ProbSeqMatrixUtils
import Sequence.Matrix.Operations.Filtering
import qualified SparseMatrix as M
import Control.Monad
import Data.List
import Data.Word
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad.Random

import Sequence
import ArbitraryConstructors
import ConstructorSampling

run = defaultMain $ testGroup "" [
    localOption (QuickCheckTests 1000) constructorMatchPropTest
  ]

constructorMatchPropTest = testProperty "random matrix matches" $
  \v -> ioProperty $ constructorMatchProp' v

constructorMatchProp' :: Constructor (Tree Word8) (MatSeq (Tree Word8)) -> IO Bool
constructorMatchProp' = constructorMatchProp

constructorMatchProp :: (Eq s, Show s)
                     => Constructor (Tree s) (MatSeq (Tree s)) -> IO Bool
constructorMatchProp c = do
  sample <- sampleSeq vecUniformDist (buildConstructor c)
  return . uncurry withinEps $ sampleMatchesProp c sample

  {-
  random matrix matches: ([],0)
FAIL
    *** Failed! Falsifiable (after 1 test): 
    eitherOr 0.8448821649202728 (MatSeq {trans = SM SparseMatrix 3x4
    0.0	0.0	1.0	0.0
    0.0	0.0	0.6170560179033455	0.3829439820966546
    0.0	0.0	0.0	1.0
    , stateLabels = []}) (MatSeq {trans = SM SparseMatrix 2x3
    0.2368932015716687	0.44982368242696885	0.31328311600136244
    0.0	0.0	1.0
    , stateLabels = []})
    3.674636057290268e-2 /= 0.0
    Use --quickcheck-replay '0 TFGenR 00000354866EF81D0000000002FAF080000000000000E3870000026977849800 0 72057594037927936 57 0' to reproduce.

1 out of 1 tests failed (0.01s)
*** Exception: ExitFailure 1
    eitherOr 0.9858087023386917 (MatSeq {trans = SM SparseMatrix 1x1
    1.0
    , stateLabels = []}) (MatSeq {trans = SM SparseMatrix 3x4
    0.6160788046653211	0.2821307775736326	0.10179041776104641	0.0
    0.3652553610890973	0.389766055293148	3.4428815212243266e-2	0.2105497684055116
    0.4428493941253989	4.0857327017206416e-2	0.4967424098833574	1.9550868974037187e-2
    , stateLabels = []})
    0.9945516600385202 /= 8.742957699828604e-3
    Use --quickcheck-replay '1 TFGenR 00000354866EF81D0000000002FAF080000000000000E3870000026977849800 0 432345564227567616 59 0' to reproduce.

1 out of 1 tests failed (0.01s)
*** Exception: ExitFailure 1

stateSequenceProbability (V.empty, 0) (buildConstructor $ EitherOr 0.98 (buildMatSeq emptySequence) $ MatSeq (M.sparseMx [[0.6160788046653211,0.2821307775736326,0.10179041776104641,0.0],[0.3652553610890973,0.389766055293148,3.4428815212243266e-2,0.2105497684055116], [0.4428493941253989,4.0857327017206416e-2,0.4967424098833574,1.9550868974037187e-2]]) V.empty)
-}

-- sampleMatchesProp (UniformDistOver [buildMatSeq (state 0)]) (V.fromList [0], 0)

sampleMatchesProp :: (Eq s) => Constructor s (MatSeq s) -> (Vector s, Int) -> (Double, Double)
sampleMatchesProp c s = (submatrixP, matSeqP)
  where submatrixP = sampleConstructor c s
        matSeqP = stateSequenceProbability s (buildConstructor c)

instance (Arbitrary s) => Arbitrary (Constructor (Tree s) (MatSeq (Tree Word8))) where
  arbitrary = do
    constructor <- oneof $ [
        arbitraryEitherOr
      , arbitraryAndThen
      , arbitraryPossibly
      , arbitraryFiniteDistOver
      , arbitraryReverseSequence
      , arbitraryCollapse
      ]
    forM constructor $ \() -> do
      trans <- arbitrary
      labels <- V.fromList <$> replicateM (nStates trans) arbitrary
      let stateLabels = V.map (, StateTag 0 []) labels
      return . filterUnreachableStates $ MatSeq { trans = trans, stateLabels = stateLabels }


instance Arbitrary SparseMatrix where
  arbitrary = do
    nStates <- choose (0, 6)
    nEnds <- choose (1, 4)
    density <- choose (10, 80)
    let dims = (nStates + 1, nStates + nEnds)
    randomMatrix dims density

randomMatrix :: (Int, Int) -> Int -> Gen SparseMatrix
randomMatrix (h, w) percentDensity = do
  nonzeros <- forM [1..h] (\r -> (r,) <$> choose (h, w))
  let randomSparseElem = frequency
        [ (percentDensity, randomElem)
        , (100 - percentDensity, return 0)]
      randomElem = choose (0.01, 1)
      buildElem loc = if find (== loc) nonzeros == Nothing
                      then randomSparseElem
                      else randomElem
  normalize <$> buildMatrixM (h, w) buildElem
