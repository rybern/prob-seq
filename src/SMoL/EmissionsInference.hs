{-# LANGUAGE RankNTypes, TupleSections, ViewPatterns, RecordWildCards, OverloadedLists #-}
module SMoL.EmissionsInference where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Matrix
import Control.Applicative
import SMoL
import Data.List
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

sampleNormalSequence :: (Ord a, Floating b, Real b) => Map a (b, b) -> Vector a -> State StdGen (Vector b)
sampleNormalSequence normals seq =
  V.mapM (\(m, s) -> realToFrac <$> sampleNormal (realToFrac m :: Double, realToFrac s)) normalsSeq
  where (Just normalsSeq) = V.mapM (flip Map.lookup normals) seq

symbolDist :: (Floating b, Eq b) => Vector (b, b) -> Vector b -> VecMat b
symbolDist normals seq = flip V.map seq
  (\datum -> V.map (\normal -> normalDensity normal datum) normals)

-- this should take Vector b, the data to take the likelihood over
testLikelihoodNormals :: (Eq a, Ord a, Show a, Floating b, Eq b, Show b, Real b)
                      => ProbSeq a -> Vector b -> Map a (b, b) -> b
testLikelihoodNormals ps dat modelNormals = infer likelihood ems ms
  where ms = compileSMoL ps
        allSyms = nub . V.toList . V.map stateLabel . stateLabels $ ms
        ems = simulateEmissionsNormals dat allSyms modelNormals

string = replicate 100 'a' ++ replicate 100 'b'

-- next: separate out sampling from fitting
--       multiple samples, jointly, adding loglike

meanLL :: (Floating b, Real b, Show b, Real c, Ord a, Show a)
       => ProbSeq a -> Vector c -> Map a b -> b
meanLL ps (V.map realToFrac -> dat) means =
  log $ testLikelihoodNormals ps dat (Map.map (,1) means)

meanLLs :: (Floating b, Real b, Show b, Real c, Ord a, Show a)
        => ProbSeq a -> [Vector c] -> Map a b -> b
meanLLs ps dats init =
  sum . map (\dat -> meanLL ps dat init) $ dats

meanPost :: (Floating b, Real b, Show b, Real c, Ord a, Show a)
         => ProbSeq a -> Vector c -> Map a (c, c) -> Map a b -> b
meanPost ps dat (Map.map (\(m,s) -> (realToFrac m, realToFrac s)) -> priors) means =
  meanLL ps dat means + sum (Map.intersectionWith (\prior mean -> log $ normalDensity prior mean) priors means)

meanPosts :: (Floating b, Real b, Show b, Real c, Ord a, Show a)
          => ProbSeq a -> [Vector c] -> Map a (c, c) -> Map a b -> b
meanPosts ps dats priors init =
  sum . map (\dat -> meanPost ps dat priors init) $ dats

meanDescent1 model = meanDescent' 100 10 model
  (Map.fromList [('a',0), ('b',0)])
  (Map.fromList [('a', (0,100)), ('b', (0,100))])
  (Map.fromList [('a',-1),('b',1)])

meanDescent :: (Ord a, Show a) => Int -> Int -> ProbSeq a -> Map a Double -> Map a (Double, Double) -> Map a Double -> IO [Map a Double]
meanDescent nSeqs nSamples model modelMeans modelPriors generatingMeans = do
  let ms = compileSMoL model
      generatingNorms = Map.map (,1) generatingMeans
  seqs <- replicateM nSeqs $ fst <$> sampleSeq vecDist ms
  gen <- getStdGen
  let labeledDats = flip evalState gen $
        (concat <$>) . forM seqs $ \seq ->
          (map (seq,) <$>) . replicateM nSamples $
            sampleNormalSequence generatingNorms seq
      dats = map snd labeledDats
      trueMeans = calcTrueMeans labeledDats
  --putStrLn . ("True means: " ++) . intercalate "\t" . map (\(k, v) -> show k ++ ":" ++ show v) $ Map.toList trueMeans
  return $ gradientAscent (meanPosts model dats modelPriors) (Map.map realToFrac modelMeans)

-- Important:
-- This is maximum likelihood, we need maximum posterior. Need to include prior distributions on means. Easy.

calcTrueMeans :: (Fractional b, Ord a) => [(Vector a, Vector b)] -> Map a b
calcTrueMeans labaledDats =
    Map.map mean
  . Map.fromListWith (++)
  . map (\(a, b) -> (a, [b]))
  . concat
  . map (V.toList . uncurry V.zip)
  $ labaledDats
  where mean xs = sum xs / fromIntegral (length xs)

meanDescent' :: (Ord a, Show a) => Int -> Int -> ProbSeq a -> Map a Double -> Map a (Double, Double) -> Map a Double -> IO ()
meanDescent' nSeqs nSamples model modelMeans modelPriors generatingMeans = do
  putStrLn . intercalate "\t" . map (\(k, v) -> show k ++ ":" ++ show v) $ Map.toList modelMeans
  updates <- meanDescent nSeqs nSamples model modelMeans modelPriors generatingMeans
  forM_ updates $ \update -> do
    putStrLn . intercalate "\t" . map (\(k, v) -> show k ++ ":" ++ show v) $ Map.toList update

testLikelihood :: (Eq a, Ord a, Show a) => ProbSeq a -> Prob -> [a] -> Double
testLikelihood ps p syms = infer likelihood ems ms
  where ms = compileSMoL ps
        allSyms = nub . V.toList . V.map stateLabel . stateLabels $ ms
        ems = simulateEmissionsUniform allSyms p (V.fromList syms)

simulateEmissionsNormals :: (Eq b, Floating b, Real b, Ord a)
                         => Vector b -> [a] -> Map a (b, b) -> Emissions b a
simulateEmissionsNormals dat allSyms modelNormals = Emissions {
    emissionMat = symbolDist allNormals dat
  , indexMap = Map.fromList $ zip allSyms [0..]
  }
  where (Just allNormals) = V.mapM (\sym -> Map.lookup sym modelNormals) (V.fromList allSyms)

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
