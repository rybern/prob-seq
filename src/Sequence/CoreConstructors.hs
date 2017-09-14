{-# LANGUAGE DeriveFunctor #-}
module Sequence.CoreConstructors
  (
    CoreConstructor' (..)
  , CoreConstructor (..)
  , CoreProbSeq (..)
  , toCore
  ) where

import Sequence.Matrix.Types
import Data.Vector (Vector)
import Data.Fix
import Sequence.Constructors
import Unsafe.Coerce

data CoreConstructor' a s t =
    CEmptySequence
  | CDeterministicSequence (Vector s)
  | CSkip Int
  | CMatrixForm (MatSeq s)
  | CEitherOr Prob t t
  | CAndThen t t
  | CIntersperse t t
  | CReverseSequence t
  | CCollapse (Vector s -> s) Int t
  | CGeometricRepeat Prob t
  | CId a
  deriving (Functor)

type CoreConstructor s t = CoreConstructor' t s t
type CoreProbSeq a s = Fix (CoreConstructor' a s)

constrToCore :: CoreConstructor' a s (Constructor s a)
             -> CoreProbSeq a s
constrToCore = Fix . fmap toCore

cid :: a -> CoreProbSeq a s
cid = Fix . CId

coreFiniteDistOver :: [(CoreProbSeq a s, Prob)] -> CoreProbSeq a s
coreFiniteDistOver [(a, _)] = a
coreFiniteDistOver ((a, p) : rest) = Fix $ CEitherOr p a $
  coreFiniteDistOver (map (\(a', p') -> (a', p' / (1 - p))) rest)

toCore :: Constructor s a
       -> CoreProbSeq a s
toCore EmptySequence = Fix $ CEmptySequence
toCore (DeterministicSequence v) = Fix $ CDeterministicSequence v
toCore (MatrixForm v) = Fix $ CMatrixForm v
toCore (Skip n) = Fix $ CSkip n
toCore (EitherOr p a1 a2) = Fix $ CEitherOr p (cid a1) (cid a2)
toCore (AndThen a1 a2) = Fix $ CAndThen (cid a1) (cid a2)
toCore (ReverseSequence a1) = Fix $ CReverseSequence (cid a1)
toCore (GeometricRepeat p a1) = Fix $ CGeometricRepeat p (cid a1)
toCore (Collapse _ f n a1) = Fix $ CCollapse f n (cid a1)
toCore (SkipDist dist a1) =
  let skipSeq = coreFiniteDistOver $ zip (Fix CEmptySequence : map (Fix . CSkip) [1..]) dist
  in Fix $ CIntersperse skipSeq (cid a1)
toCore (UniformDistOver seqs) =
  let uniform = recip . fromIntegral . length $ seqs
  in toCore $ FiniteDistOver $ map (\seq -> (seq, uniform)) seqs
toCore (FiniteDistOver as) = coreFiniteDistOver . map (\(a, p) -> (cid a, p)) $ as
toCore (UniformDistRepeat n s) =
  let uniform = recip . fromIntegral . succ $ n
  in toCore $ FiniteDistRepeat (replicate n uniform) s
toCore (FiniteDistRepeat [] _) = Fix $ CEmptySequence
toCore (FiniteDistRepeat ps a) =
  Fix $ CEitherOr p (Fix CEmptySequence) (Fix $ CAndThen (cid a) (toCore $ FiniteDistRepeat rest a))
  where (p:rest) = normalize ps
toCore (Possibly p a) = Fix $ CEitherOr p (cid a) (Fix $ CEmptySequence)
toCore (Series []) = Fix $ CEmptySequence
toCore (Series [a]) = cid a
toCore (Series as) = Fix $ CAndThen (toCore (Series leftAs)) (toCore (Series rightAs))
  where (leftAs, rightAs) = splitAt (length as `div` 2) as
toCore (Repeat n a) = toCore (Series (replicate n a))

normalize :: (Traversable t) => t Prob -> t Prob
normalize t = let s = sum t in if s == 0
                               then (const (1 / fromIntegral (length t))) <$> t
                               else (/ s) <$> t
