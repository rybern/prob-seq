{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}
module Sequence.Matrix ( buildConstructor
                       --, buildMatSeqTree
                       , buildMatSeq
                       , module Sequence.Matrix.Types ) where

import Sequence.Constructors
import Sequence.Matrix.Types
import Sequence.Matrix.ProbSeqMatrixUtils
import qualified Sequence.Matrix.Operations as Ops
import Data.Fix
import Data.Vector (Vector)
import Sequence.Constructors
import Unsafe.Coerce
import Control.Monad.State
import qualified SparseMatrix as M

import Control.Parallel.Strategies

buildMatSeq :: (Eq s) => ProbSeq s -> MatSeq s
buildMatSeq = Ops.filterUnreachableStates
            . cata (buildConstructor $| parTraversable rpar)
            . unProbSeqWith

  {-
buildMatSeqTree :: (Eq s) => ProbSeq s -> ProbSeqWith s (MatSeq s)
buildMatSeqTree =
  ProbSeqWith
  . Fix
  . cata (\constr -> ConstructorWith {
             with =
                 Ops.filterUnreachableStates $ buildConstructor (with <$> constr)
             , constructor = Fix <$> constr
             } )
  . unProbSeqWith
-}

buildConstructor :: (Eq s) => ConstructorWith s (Maybe Int) (MatSeq s) -> MatSeq s
buildConstructor (ConstructorWith mTagID EmptySequence) = Ops.emptySequence
buildConstructor (ConstructorWith mTagID (State s)) = Ops.state s
buildConstructor (ConstructorWith mTagID (MatrixForm v)) = v
buildConstructor (ConstructorWith mTagID (Skip n)) = Ops.skip n
buildConstructor (ConstructorWith mTagID (SkipDist ps a)) = Ops.skipDist ps a
buildConstructor (ConstructorWith mTagID (EitherOr p a1 a2)) = Ops.eitherOr p a1 a2
buildConstructor (ConstructorWith mTagID (AndThen a1 a2)) = Ops.andThen a1 a2
buildConstructor (ConstructorWith mTagID (AndThen' a1 a2)) = Ops.andThen' a1 a2
buildConstructor (ConstructorWith mTagID (ReverseSequence a1)) = Ops.reverseSequence a1
buildConstructor (ConstructorWith mTagID (GeometricRepeat p a1)) = Ops.geometricRepeat p a1
buildConstructor (ConstructorWith mTagID (Collapse _ f n a1)) = Ops.collapse f n a1

buildConstructor (ConstructorWith mTagID (UniformDistOver seqs)) =
  let uniform = recip . fromIntegral . length $ seqs
  in buildConstructor $ ConstructorWith Nothing (FiniteDistOver $ map (\seq -> (seq, uniform)) seqs)
buildConstructor (ConstructorWith mTagID (FiniteDistOver pairs)) = f . map (\(i, (a, p)) -> (appendLabelSeq i a, p)) . zip [0..] $ pairs
  where f [(a, _)] = a
        f ((a, p) : rest) = removeLabelSeq . Ops.eitherOr p a $
          f $ map (\(a', p') -> (a', p' / (1 - p))) rest
buildConstructor (ConstructorWith mTagID (UniformDistRepeat n s)) =
  let uniform = recip . fromIntegral . succ $ n
  in buildConstructor $ ConstructorWith Nothing (FiniteDistRepeat (replicate (n+1) uniform) s)
buildConstructor (ConstructorWith mTagID (UniformDistRepeat' n s)) =
  let uniform = recip . fromIntegral . succ $ n
  in buildConstructor $ ConstructorWith Nothing (FiniteDistRepeat' (replicate (n+1) uniform) s)
buildConstructor (ConstructorWith mTagID (FiniteDistRepeat ps a)) = f 0 ps a
  where f _ [] _ = Ops.emptySequence
        f ix ps a = removeLabelSeq $ Ops.eitherOr p
          Ops.emptySequence
          (removeLabelSeq $ Ops.andThen
            (appendLabelSeq ix a)
            (f (ix + 1) rest a))
          where (p:rest) = normalize ps
buildConstructor (ConstructorWith mTagID (FiniteDistRepeat' ps a)) = f 0 ps a
  where f _ [] _ = Ops.emptySequence
        f ix ps a = removeLabelSeq $ Ops.eitherOr p
          Ops.emptySequence
          (removeLabelSeq $ Ops.andThen'
            (appendLabelSeq ix a)
            (f (ix + 1) rest a))
          where (p:rest) = normalize ps
buildConstructor (ConstructorWith mTagID (Possibly p a)) = Ops.eitherOr p a Ops.emptySequence
buildConstructor (ConstructorWith mTagID (Series as)) = f 0 as
  where f _ [] = Ops.emptySequence
        f ix [a] = appendLabelSeq ix a
        f ix as = let (leftAs, rightAs) = splitAt (length as `div` 2) as
                  in removeLabelSeq $ Ops.andThen'
                     (f ix leftAs)
                     (f (ix + length leftAs) rightAs)
buildConstructor (ConstructorWith mTagID (Series' [])) = Ops.emptySequence
buildConstructor (ConstructorWith mTagID (Series' as)) =
  foldl1 (\a b -> removeLabelSeq $ Ops.andThen' a b) (zipWith appendLabelSeq [0..] as)
buildConstructor (ConstructorWith mTagID (Repeat n a)) =
  buildConstructor (ConstructorWith Nothing (Series (replicate n a)))
buildConstructor (ConstructorWith mTagID (Repeat' n a)) =
  buildConstructor (ConstructorWith Nothing (Series' (replicate n a)))

normalize :: (Traversable t) => t Prob -> t Prob
normalize t = let s = sum t in if s == 0
                               then (const (1 / fromIntegral (length t))) <$> t
                               else (/ s) <$> t
