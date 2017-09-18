{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}
module Sequence.Matrix ( buildConstructor
                       , buildMatSeqTree
                       , buildMatSeq
                       , module Sequence.Matrix.Types ) where

import Sequence.Constructors
import Sequence.Matrix.Types
import Sequence.Matrix.ProbSeqMatrixUtils
import Sequence.Matrix.Operations
import Data.Fix
import Data.Vector (Vector)
import Sequence.Constructors
import Unsafe.Coerce
import Control.Monad.State

import Control.Parallel.Strategies

buildMatSeq :: (Eq s) => ProbSeq s -> MatSeq s
buildMatSeq = filterUnreachableStates . cata (buildConstructor $| parTraversable rpar)

buildMatSeqTree :: (Eq s) => ProbSeq s -> ProbSeqWith s (MatSeq s)
buildMatSeqTree =
  ProbSeqWith . Fix . cata (\constr -> ConstructorWith {
                               with =
                                   filterUnreachableStates $ buildConstructor (with <$> constr)
                               , constructor = Fix <$> constr
                               } )

buildConstructor :: (Eq s) => Constructor s (MatSeq s) -> MatSeq s
buildConstructor EmptySequence = emptySequence
buildConstructor (DeterministicSequence v) = deterministicSequence v
buildConstructor (MatrixForm v) = v
buildConstructor (Skip n) = skip n
buildConstructor (EitherOr p a1 a2) = eitherOr p a1 a2
buildConstructor (AndThen a1 a2) = andThen a1 a2
buildConstructor (ReverseSequence a1) = reverseSequence a1
buildConstructor (GeometricRepeat p a1) = geometricRepeat p a1
buildConstructor (Collapse _ f n a1) = collapse f n a1

buildConstructor (UniformDistOver seqs) =
  let uniform = recip . fromIntegral . length $ seqs
  in buildConstructor $ FiniteDistOver $ map (\seq -> (seq, uniform)) seqs
buildConstructor (FiniteDistOver pairs) = f . map (\(i, (a, p)) -> (appendLabelSeq i a, p)) . zip [0..] $ pairs
  where f [(a, _)] = a
        f ((a, p) : rest) = removeLabelSeq . eitherOr p a $
          f $ map (\(a', p') -> (a', p' / (1 - p))) rest
buildConstructor (UniformDistRepeat n s) =
  let uniform = recip . fromIntegral . succ $ n
  in buildConstructor $ FiniteDistRepeat (replicate n uniform) s
buildConstructor (FiniteDistRepeat ps a) = f 0 ps a
  where f _ [] _ = emptySequence
        f ix ps a = removeLabelSeq $ eitherOr p
          emptySequence
          (removeLabelSeq $ andThen
            (appendLabelSeq ix a)
            (f (ix + 1) rest a))
          where (p:rest) = normalize ps
buildConstructor (Possibly p a) = eitherOr p a emptySequence
buildConstructor (Series as) = f 0 as
  where f _ [] = emptySequence
        f ix [a] = appendLabelSeq ix a
        f ix as = let (leftAs, rightAs) = splitAt (length as `div` 2) as
                  in removeLabelSeq $ andThen
                     (f ix leftAs)
                     (f (ix + length leftAs) rightAs)
buildConstructor (Repeat n a) = buildConstructor (Series (replicate n a))

normalize :: (Traversable t) => t Prob -> t Prob
normalize t = let s = sum t in if s == 0
                               then (const (1 / fromIntegral (length t))) <$> t
                               else (/ s) <$> t
