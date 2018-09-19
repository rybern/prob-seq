{-# LANGUAGE RecordWildCards #-}
module TestAST where

import Control.Monad.Random
import Data.Vector (Vector)
import Data.Monoid
import Data.Maybe
import Data.Foldable

import Sequence
import Sequence.Constructors

import ConstructorSampling
import Debug.Trace

data CompareError s = CompareError {
    path :: (Vector s, Int)
  , probMatrix :: Prob
  , probSubmatrices :: Prob
  , matrices :: ConstructorWith s (MatSeq s) (MatSeq s)
  , samples :: [(Vector s, Int)]
  } deriving Show


checkConstructorExact' :: (Show s, Eq s)
                      => Int
                      -> ConstructorWith s (MatSeq s) (MatSeq s)
                      -> IO (Maybe (CompareError s))
checkConstructorExact' nSamples c@(ConstructorWith {..}) = do
  samples <- replicateM nSamples $ sampleSeq vecUniformDist with
  print samples
  print $ "constructor: " ++  show constructor
  print $ "with: " ++  show with
  print $ "ssp " ++ (show $ stateSequenceProbability (head samples) with)
  print $ "sc " ++ (show $ constructorSeqProb (fmap sequenceDist constructor) (head samples))
  -- endAfterStepsProbability ms
  return . getFirst . mconcat . map First . flip map samples $ \s ->
    let pMat = stateSequenceProbability s with
        pSubmats = sampleConstructor constructor s
    in if pSubmats `withinEps` pMat
       then Nothing
       else Just $ CompareError {
        path = s
      , probMatrix = pMat
      , probSubmatrices = pSubmats
      , matrices = c
      , samples = samples
      }

checkConstructorExact :: (Eq s, MonadRandom m)
                      => Int
                      -> ConstructorWith s (MatSeq s) (MatSeq s)
                      -> m (Maybe (CompareError s))
checkConstructorExact nSamples c@(ConstructorWith {..}) = do
  samples <- replicateM nSamples $ sampleSeq vecUniformDist with
  return . getFirst . mconcat . map First . flip map samples $ \s ->
    let pMat = stateSequenceProbability s with
        pSubmats = sampleConstructor constructor s
    in if pSubmats `withinEps` pMat
       then Nothing
       else Just $ CompareError {
        path = s
      , probMatrix = pMat
      , probSubmatrices = pSubmats
      , matrices = c
      , samples = samples
      }


checkProbSeqExact :: (Eq s, MonadRandom m)
                  => Int
                  -> ProbSeqWith s (MatSeq s)
                  -> m (ProbSeqWith s (Maybe (CompareError s)))
checkProbSeqExact nSamples = sequence . mapWith (checkConstructorExact nSamples)

listFailures :: ProbSeqWith s (Maybe (CompareError s))
            -> [CompareError s]
listFailures = catMaybes . toList

getProbSeqFailures :: (Eq s, MonadRandom m) =>
                      ProbSeq s ->
                      m [CompareError s]
getProbSeqFailures ps = do
  let matSeq = buildMatSeqTree ps
  errors <- checkProbSeqExact 10 matSeq
  return $ listFailures errors
