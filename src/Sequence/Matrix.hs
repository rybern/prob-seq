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

rewriteTags :: (Constructor s (MatSeq s) -> MatSeq s)
            -> Constructor s (MatSeq s)
            -> MatSeq s
rewriteTags f = removeLabelSeq . f . mapIx (\(m, i) -> appendLabelSeq i m)

mapIx :: (Traversable t, Bounded i, Enum i)
      => ((a, i) -> b) -> t a -> t b
mapIx f t = flip evalState minBound $ mapM iterator t
  where iterator a = do
          ix <- get
          let b = f (a, ix)
          put (succ ix)
          return b

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
buildConstructor (FiniteDistOver [(a, _)]) = a
buildConstructor (FiniteDistOver ((a, p) : rest)) = eitherOr p a $
  buildConstructor (FiniteDistOver $ map (\(a', p') -> (a', p' / (1 - p))) rest)
buildConstructor (UniformDistRepeat n s) =
  let uniform = recip . fromIntegral . succ $ n
  in buildConstructor $ FiniteDistRepeat (replicate n uniform) s
buildConstructor (FiniteDistRepeat [] _) = emptySequence
buildConstructor (FiniteDistRepeat ps a) =
  eitherOr p (emptySequence) (andThen a (buildConstructor $ FiniteDistRepeat rest a))
  where (p:rest) = normalize ps
buildConstructor (Possibly p a) = eitherOr p a emptySequence
buildConstructor (Series []) = emptySequence
buildConstructor (Series [a]) = a
buildConstructor (Series as) = andThen (buildConstructor (Series leftAs)) (buildConstructor (Series rightAs))
  where (leftAs, rightAs) = splitAt (length as `div` 2) as
buildConstructor (Repeat n a) = buildConstructor (Series (replicate n a))

normalize :: (Traversable t) => t Prob -> t Prob
normalize t = let s = sum t in if s == 0
                               then (const (1 / fromIntegral (length t))) <$> t
                               else (/ s) <$> t

buildMatSeq :: (Eq s) => ProbSeq s -> MatSeq s
buildMatSeq = filterUnreachableStates . cata (buildConstructor $| parTraversable rpar)

buildMatSeqTree :: (Eq s) => ProbSeq s -> ProbSeqWith s (MatSeq s)
buildMatSeqTree =
  ProbSeqWith . Fix . cata (\constr -> ConstructorWith {
                               with =
                                   filterUnreachableStates $ buildConstructor (with <$> constr)
                               , constructor = Fix <$> constr
                               } )
