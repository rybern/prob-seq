{-# LANGUAGE RankNTypes, OverloadedLists, TupleSections, RecordWildCards, ViewPatterns #-}
module ConstructorSampling where

import Control.Monad.Random
import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.List
import Data.Monoid
import Data.Function

import Sequence.Matrix
import Sequence.Matrix.Types
import Sequence.Matrix.Sampling
import Sequence.Matrix.Emissions
import Sequence.Matrix.Operations
import Sequence.Constructors

epsilon :: Prob
epsilon = 1e-10

withinEps :: Prob -> Prob -> Bool
withinEps a b = abs (a - b) < epsilon

sampleConstructor :: Eq s
                  => Constructor s (MatSeq s)
                  -> (Vector s, Int)
                  -> Prob
sampleConstructor constructor s = constructorSeqProb (fmap sequenceDist constructor) s

data SequenceDist s = SeqDist {
    seqProb :: (Vector s, Int) -> Prob
  , possibleSkips :: [Int]
  , seqSuffixProb :: Int -> (Vector s, Int) -> Prob
  , endAfterN :: Int -> Int -> Prob
  }

sequenceDist :: (Eq s)
             => MatSeq s
             -> SequenceDist s
sequenceDist ms = SeqDist {
    seqProb = \s -> stateSequenceProbability s ms
  , possibleSkips = reachableSkips (trans ms)
  , seqSuffixProb = \i s -> sequenceSuffixProbability i s ms
  , endAfterN = endAfterStepsProbability ms
  }

constructorSeqProb :: (Eq s)
                  => Constructor s (SequenceDist s)
                  -> (Vector s, Int)
                  -> Prob
constructorSeqProb EmptySequence s = if s == (V.empty, 0) then 1 else 0
constructorSeqProb (State s') s = if s == (V.singleton s', 0) then 1 else 0
constructorSeqProb (Skip n) s = if s == (V.empty, n) then 1 else 0
constructorSeqProb (EitherOr p m1 m2) s = p * seqProb m1 s + (1 - p) * seqProb m2 s
constructorSeqProb (AndThen m1 m2) (seq, sk) = sum $ do
  (left, right) <- map (\n -> V.splitAt n seq) [0..V.length seq]
  skipLeft <- possibleSkips m1
  let pLeft = seqProb m1 (left, skipLeft)
  guard $ pLeft > 0
  let pRight = seqSuffixProb m2 skipLeft (right, sk)
  guard $ pRight > 0
  return $ pRight * pLeft
constructorSeqProb (Possibly p m) s = if s == (V.empty, 0)
                                     then (1 - p) + p * seqProb m s
                                     else p * seqProb m s
constructorSeqProb (UniformDistOver ms) s = let p = 1 / fromIntegral (length ms) in sum $ map (\m -> p * seqProb m s) ms
constructorSeqProb (FiniteDistOver ms) s = sum $ map (\(m, p) -> p * seqProb m s) ms
constructorSeqProb (Collapse split _ n m) (V.map split -> ss, sk) =
  if not valid
  then 0.0
  else if V.null ss
    -- this needs to exactly reimplement what's in collapse; total probability of ending in the first n-1 steps
       then endAfterN m n sk
       else let recovered = V.head ss <> (V.map V.last (V.tail ss))
            in seqProb m (recovered, sk)

  where validPair s1 s2 = V.tail s1 == V.init s2
        validList (t1:t2:rest) = validPair t1 t2 && validList (t2:rest)
        validList _ = True
        valid = validList (V.toList ss)

        -- if ss is empty, recovered could be anything of length 0 to n-1
        -- probability of an empty suffix after n-1 steps?

constructorSeqProb (ReverseSequence m) (s, sk) =
  if sk /= 0
  then 0.0
  else sum $ [ seqProb m (V.reverse s, sk')
             | sk' <- possibleSkips m
             ]

--constructorSeqProb (GeometricRepeat p m) s = undefined

{-
sampleConstructor sampleList (FiniteDistRepeat ps m) = do
  (n, p) <- sampleList (zip [0..] ps)
  samples <- replicateM n (sampleSeq vecUniformDist m)

  let join (s1, sk1) (s2, sk2) =
        let overflow = s1 - V.length sk2
        in if overflow > 0
           then (s1, overflow + sk2)
           else (s1 <> V.drop sk1 s2, sk2)
      seq = foldl1' join samples

sampleConstructor sampleList (UniformDistRepeat n m) = undefined
sampleConstructor sampleList (Collapse n m) = undefined
-}

-- sampleSeq hangs on sampling empty
