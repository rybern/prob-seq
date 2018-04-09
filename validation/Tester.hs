{-# LANGUAGE OverloadedLists, RecordWildCards #-}
module Tester where

import Sequence
import EmissionIO
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad.Random
import qualified SHMM as SHMM
import Sequence.Matrix.ProbSeqMatrixUtils
import Math.LinearAlgebra.Sparse.Matrix hiding (trans)

import MinION
import GeneralizedSNP
import SNP

type StateIx = Int
type RMSE = Double

data SimulationResult s = SimRes {
    states :: Vector StateIx
  , rmse :: RMSE
  , misclassify :: Double
  , post :: Emissions
  , maxLabels :: Vector s
  , trueLabels :: Vector s
  } deriving Show

prettyPrintSimulationResult :: (Show s) => Bool -> SimulationResult s -> IO ()
prettyPrintSimulationResult showPost (SimRes {..}) = do
  putStrLn $ "True state labels:"
  putStrLn $ "\t<" ++ intercalate "," (V.toList . V.map show $ trueLabels) ++ ">"
  putStrLn $ "Max state labels:"
  putStrLn $ "\t<" ++ intercalate "," (V.toList . V.map show $ maxLabels) ++ ">"
  putStrLn $ "RMSE: " ++ show rmse
  putStrLn $ "% misclassified: " ++ show (100*misclassify)
  when showPost $ do
    putStrLn $ "posterior:"
    prettyPrintEmissions post

prettyPrintEmissions :: Emissions -> IO ()
prettyPrintEmissions ems = forM_ ems $ putStrLn . ('\t':) . intercalate "\t" . V.toList . V.map show

simulationSimple = simulation simpleNoise

simulationUniform p = simulation (uniformNoise p)

simulation :: (Int -> StateIx -> Vector Prob) -> MatSeq b -> IO (SimulationResult b)
simulation noise ms = do
  (observations, truth) <- simulate noise ms
  estimated <- posterior ms observations
  return $ SimRes {
      states = truth
    , rmse = scoreRMSE truth estimated
    , misclassify = scoreMisclassify truth estimated
    , post = estimated
    , maxLabels = maxLabelPath ms estimated
    , trueLabels = V.map (fst . (stateLabels ms V.!)) truth
    }

posterior :: MatSeq a -> Emissions -> IO Emissions
posterior ms observations = do
  let ns = (nStates (trans ms))
      permutation = [0..ns - 1]
      triples = matSeqTriples ms
  post <- SHMM.shmmFull ns triples observations permutation
  let post' = V.map (V.tail . V.init) $ post
  return post




matSeqTriples :: MatSeq a
              -> [(Int, Int, Double)]
matSeqTriples = map (\((r, c), p) -> (r - 1, c - 1, p)) . tail . toAssocList . cleanTrans . trans

cleanTrans :: Trans -> Trans
cleanTrans = addStartColumn . collapseEnds

simulate :: (MonadRandom m) => (Int -> StateIx -> Vector Prob) -> MatSeq a -> m (Emissions, Vector StateIx)
simulate observe ms = do
  (ixs, _) <- sampleSeqIxs vecDist ms
  let len = length . stateLabels $ ms
      observations = V.map (observe len) ixs
  return (observations, ixs)

uniformNoise :: Prob -> Int -> StateIx -> Vector Prob
uniformNoise pnt len ix = V.replicate len other V.// [(ix, pnt)]
  where other = (1 - pnt) / fromIntegral len

simpleNoise :: Int -> StateIx -> Vector Prob
simpleNoise = onehot

onehot :: Int -> Int -> Vector Prob
onehot len ix = V.replicate len 0 V.// [(ix, 1)]

meanScoreByRow :: (StateIx -> Vector Prob -> Double) -> Vector StateIx -> Emissions -> Double
meanScoreByRow byRow ixs obs = (\v -> sum v / fromIntegral (V.length v)) . V.map (uncurry byRow) $ V.zip ixs obs

scoreMisclassify :: Vector StateIx -> Emissions -> Double
scoreMisclassify = meanScoreByRow score
  where score ix ob = if V.maxIndex ob == ix then 0.0 else 1.0

scoreRMSE :: Vector StateIx -> Emissions -> RMSE
scoreRMSE truth = sqrt . meanScoreByRow score truth
  where score ix = sum
                 . V.imap (\ix' v -> let t = if ix == ix' then 1.0 else 0.0
                                     in (v - t) ** 2)

maxIxPath :: Emissions -> Vector StateIx
maxIxPath = V.map V.maxIndex

maxLabelPath :: MatSeq a -> Emissions -> Vector a
maxLabelPath ms = V.map (fst . (stateLabels ms V.!)) . maxIxPath

basic :: MatSeq Int
basic = buildMatSeq $ uniformDistOver [ series . map state $ [1..5]
                                      , series . map state $ [6..10]
                                      , series . map state $ [11..15]]

window :: MatSeq String
window = buildMatSeq . minion . series $ [
    series' "hello "
  , eitherOr 0.5 (series' "minion") (series' "world")
  , series' "!!"
  ]
  where series' = series . map (state . return)

{-
focus entirely on getting the simulation to a reasonable place.
my current theory is that there is no right noise
-}

testCalling :: IO ()
testCalling = do
  let site = Site {pos = 50, alleles = [('X', 0.5), ('Y', 0.5)] , leftFlank = "ABCDEF" , rightFlank = "fedcba"}
  let (ms, [[refIxs, altIxs]]) = snpsNTMatSeq 0 100 [site]
      regions = snpRegions 0 100 [site]

  print regions

  res <- simulationUniform 0.02 ms

  let post' = post res
      altVec = V.map (\row -> sum $ V.map (row V.!) altIxs) post'
      refVec = V.map (\row -> sum $ V.map (row V.!) refIxs) post'

  putStrLn $ "ref ixs " ++ progressString 0 (V.maxIndex refVec) (V.length refVec)
  putStrLn $ "alt ixs " ++ progressString 0 (V.maxIndex altVec) (V.length refVec)
  putStrLn $ "ref: " ++ show (sum refVec)
  putStrLn $ "alt: " ++ show (sum altVec)
  putStrLn $ "p(alt): " ++ show (sum altVec / (sum refVec + sum altVec))
  putStrLn $ show (trueLabels res)
  putStrLn $ show refVec
  putStrLn $ show altVec
  prettyPrintSimulationResult False res

progressString :: Int -> Int -> Int -> String
progressString start pnt end =
    show start ++
    "/(" ++ show pnt ++
    ", " ++ show (100 * fromIntegral (pnt - start) / fromIntegral (end - start)) ++
    "%)/" ++ show end

