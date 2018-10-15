{-# LANGUAGE DeriveFunctor, TupleSections, FlexibleContexts #-}
module Sequence.Matrix ( buildConstructor
                       , buildMatSeqTree
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
buildMatSeq = Ops.filterUnreachableStates . cata (buildConstructor $| parTraversable rpar)

buildMatSeqTree :: (Eq s) => ProbSeq s -> ProbSeqWith s (MatSeq s)
buildMatSeqTree =
  ProbSeqWith . Fix . cata (\constr -> ConstructorWith {
                               with =
                                   Ops.filterUnreachableStates $ buildConstructor (with <$> constr)
                               , constructor = Fix <$> constr
                               } )

buildConstructor :: (Eq s) => Constructor s (MatSeq s) -> MatSeq s
buildConstructor EmptySequence = Ops.emptySequence
buildConstructor (State s) = Ops.state s
buildConstructor (MatrixForm v) = v
buildConstructor (Skip n) = Ops.skip n
buildConstructor (SkipDist ps a) = Ops.skipDist ps a
buildConstructor (EitherOr t p a1 a2) = Ops.eitherOr t p a1 a2
buildConstructor (AndThen a1 a2) = Ops.andThen a1 a2
buildConstructor (AndThen' a1 a2) = Ops.andThen' a1 a2
buildConstructor (ReverseSequence a1) = Ops.reverseSequence a1
buildConstructor (GeometricRepeat p a1) = Ops.geometricRepeat p a1
buildConstructor (Collapse _ f n a1) = Ops.collapse f n a1

buildConstructor (UniformDistOver t seqs) =
  let uniform = recip . fromIntegral . length $ seqs
  in buildConstructor $ FiniteDistOver t $ map (\seq -> (seq, uniform)) seqs
-- TODO fix tag
buildConstructor (FiniteDistOver t pairs) = f . map (\(i, (a, p)) -> (appendLabelSeq' t i a, p)) . zip [0..] $ pairs
  where f [(a, _)] = a
        f ((a, p) : rest) = removeLabelSeq . Ops.eitherOr Nothing p a $
          f $ map (\(a', p') -> (a', p' / (1 - p))) rest
buildConstructor (UniformDistRepeat t n s) =
  let uniform = recip . fromIntegral . succ $ n
  in buildConstructor $ FiniteDistRepeat t (replicate (n+1) uniform) s
buildConstructor (UniformDistRepeat' t n s) =
  let uniform = recip . fromIntegral . succ $ n
  in buildConstructor $ FiniteDistRepeat' t (replicate (n+1) uniform) s
-- TODO fix tag
buildConstructor (FiniteDistRepeat t ps a) = f 0 ps a
  where f _ [] _ = Ops.emptySequence
        f ix ps a = removeLabelSeq $ Ops.eitherOr Nothing p
          Ops.emptySequence
          (removeLabelSeq $ Ops.andThen
            (appendLabelSeq' t ix a)
            (f (ix + 1) rest a))
          where (p:rest) = normalize ps
buildConstructor (FiniteDistRepeatTerm t term ps a) = f 0 ps a
  where f _ [] _ = Ops.emptySequence
        f ix ps a = removeLabelSeq $ Ops.eitherOr Nothing p
          (appendTagSeq ((,ix) <$> t) term)
          (removeLabelSeq $ Ops.andThen
            (appendLabelSeq ix a)
            (f (ix + 1) rest a))
          where (p:rest) = normalize ps
-- TODO fix tag
buildConstructor (FiniteDistRepeat' t ps a) = f 0 ps a
  where f _ [] _ = Ops.emptySequence
        f ix ps a = removeLabelSeq $ Ops.eitherOr Nothing p
          Ops.emptySequence
          (removeLabelSeq $ Ops.andThen'
            (appendLabelSeq' t ix a)
            (f (ix + 1) rest a))
          where (p:rest) = normalize ps
buildConstructor (Possibly t p a) = Ops.eitherOr t p a Ops.emptySequence
buildConstructor (Series as) = f 0 as
  where f _ [] = Ops.emptySequence
        f ix [a] = appendLabelSeq ix a
        f ix as = let (leftAs, rightAs) = splitAt (length as `div` 2) as
                  in removeLabelSeq $ Ops.andThen'
                     (f ix leftAs)
                     (f (ix + length leftAs) rightAs)
buildConstructor (Series' []) = Ops.emptySequence
buildConstructor (Series' as) =
  foldl1 (\a b -> removeLabelSeq $ Ops.andThen' a b) (zipWith appendLabelSeq [0..] as)
buildConstructor (Repeat n a) = buildConstructor (Series (replicate n a))
buildConstructor (Repeat' n a) = buildConstructor (Series' (replicate n a))
