{-# LANGUAGE RecordWildCards, OverloadedLists #-}
module SMoL.Likelihood where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Matrix
import Control.Applicative
import SMoL
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map

import Numeric.AD
import Numeric.AD.Mode.Reverse
import Data.Random.Normal
import System.Random
import Control.Monad.State

--type InferenceEngine res = Int -> [(Int, Int, Double)] -> VecMat -> Vector Int -> res

normalDensity :: (Floating a) => (a, a) -> a -> a
normalDensity (mean, stdDev) = \x ->
  (1 / sqrt (2 * pi * stdDev * stdDev))
  * exp ( - (x - mean) * (x - mean) / (2 * stdDev * stdDev))

testStates :: (Num b) => (b, b) -> Map Char (b, b)
testStates (a, b) = Map.fromList [('a', (a,1)), ('b', (b,1))]

sampleNormal :: (Random b, Floating b) => (b, b) -> State StdGen b
sampleNormal ms = do
  gen <- get
  let (res, gen') = normal' ms gen
  put gen'
  return res

sampleNormalSequence :: (Ord a, Floating b, Real b) => StdGen -> Map a (b, b) -> Vector a -> Vector b
sampleNormalSequence gen normals seq =
  flip evalState gen $ V.mapM (\(m, s) -> realToFrac <$> sampleNormal (realToFrac m :: Double, realToFrac s)) normalsSeq
  where (Just normalsSeq) = V.mapM (flip Map.lookup normals) seq

symbolDist :: (Floating b, Eq b) => Vector (b, b) -> Vector b -> VecMat b
symbolDist normals seq = flip V.map seq
  (\datum -> V.map (\normal -> normalDensity normal datum) normals)

-- this should take Vector b, the data to take the likelihood over
testLikelihoodNormals :: (Eq a, Ord a, Show a, Floating b, Eq b, Show b, Real b)
                      => StdGen -> ProbSeq a -> Vector a -> Map a (b, b) -> Map a (b, b) -> b
testLikelihoodNormals gen ps syms generatingNormals modelNormals = infer likelihood ems ms
  where ms = compileSMoL ps
        allSyms = nub . V.toList . V.map stateLabel . stateLabels $ ms
        ems = simulateEmissionsNormals gen allSyms generatingNormals modelNormals syms

string = replicate 100 'a' ++ replicate 100 'b'

-- next: separate out sampling from fitting
--       multiple samples, jointly, adding loglike

meanLL :: (Floating b, Real b, Show b)
       => StdGen -> [b] -> b
meanLL gen [x,y] =
  log $ testLikelihoodNormals gen (symbols string) (V.fromList string) (testStates (1,-1)) (testStates (x,y))

meanDiffs :: IO [Double]
meanDiffs = do
  gen <- getStdGen
  return $ grad (meanLL gen) [0,0]

meanDescent :: IO [[Double]]
meanDescent = do
  gen <- getStdGen
  return $ gradientAscent (meanLL gen) [0,0]

testLikelihood :: (Eq a, Ord a, Show a) => ProbSeq a -> Prob -> [a] -> Double
testLikelihood ps p syms = infer likelihood ems ms
  where ms = compileSMoL ps
        allSyms = nub . V.toList . V.map stateLabel . stateLabels $ ms
        ems = simulateEmissionsUniform allSyms p (V.fromList syms)

simulateEmissionsNormals :: (Eq b, Floating b, Real b, Ord a)
                         => StdGen -> [a] -> Map a (b, b) -> Map a (b, b) -> Vector a -> Emissions b a
simulateEmissionsNormals gen allSyms generatingNormals modelNormals syms = Emissions {
    emissionMat = symbolDist allNormals dat
  , indexMap = Map.fromList $ zip allSyms [0..]
  }
  where (Just allNormals) = V.mapM (\sym -> Map.lookup sym modelNormals) (V.fromList allSyms)
        dat = sampleNormalSequence gen generatingNormals syms

simulateEmissionsUniform :: (Ord a) => [a] -> Prob -> Vector a -> Emissions Double a
simulateEmissionsUniform allSym p = simulateEmissions allSym toEm
  where toEm nVal ix = V.replicate nVal ((1-p) / fromIntegral (nVal-1)) V.// [(ix, p)]

simulateEmissions :: (Ord a) => [a] -> (Int -> Int -> Vector Prob) -> Vector a -> Emissions Double a
simulateEmissions allSym toEm path = Emissions {
    emissionMat = V.map toEm' path
  , indexMap = index
  }
  where vals = allSym
        nVal = length vals
        index = Map.fromList $ zip vals [0..]
        toEm' a = let (Just ix) = Map.lookup a index
                  in toEm nVal ix

{-
+++++++++++++++++++++++  code  ++++++++++++++++++++++++
-}

likelihood :: (Fractional b, Show b)
           => InferenceEngine b b
likelihood ns triples unpermedEms perm = (\m -> getElem 1 (ncols m) m) . V.foldl foldLikelihoods initial $ ems
  where (_, initial, _, trans) = splitBlocks 1 1 $ buildDense ns triples
        ems = V.map (rowVector . permuteRow perm) unpermedEms
        elemwiseMult = elementwise (*)
        foldLikelihoods llSoFar emsRow = llSoFar `elemwiseMult` emsRow `multStd2` trans

buildDense :: (Fractional b) => Int -> [(Int, Int, Double)] -> Matrix b
buildDense ns triples = matrix (ns+2) (ns+2) (\pair -> Map.findWithDefault 0 pair valMap)
  where valMap = Map.fromList . map (\(r, c, v) -> ((r+1, c+1), realToFrac v)) $ (ns+1, ns+1, 1):triples

permuteRow :: (Fractional b) => Vector Int -> Vector b -> Vector b
permuteRow perm v = flip V.snoc 0 $ V.map (v V.!) perm
