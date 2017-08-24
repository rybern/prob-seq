{-# LANGUAGE TupleSections, RecordWildCards, DeriveTraversable, DeriveFunctor, ViewPatterns #-}
module Sequence.Constructors where

import Sequence.Matrix.Types
import Data.Vector (Vector)
import Data.Fix
import Data.List
import qualified Data.Vector as V

data Constructor s t =
    EmptySequence
  | DeterministicSequence (Vector s)
  | Skip Int
  | MatrixForm (MatSeq s)
  | EitherOr Prob t t
  | AndThen t t
  | Possibly Prob t
  | UniformDistOver [t]
  | FiniteDistOver [(t, Prob)]
  | GeometricRepeat Prob t
  | FiniteDistRepeat [Prob] t
  | UniformDistRepeat Int t
  | ReverseSequence t
  | Collapse (s -> Vector s) (Vector s -> s) Int t
  | SkipDist [Prob] t
  deriving (Functor, Foldable, Traversable)

-- should maybe write these for core constructors instead
instance (Show s, Show t) => Show (Constructor s t) where
  show EmptySequence = "<>"
  show (DeterministicSequence v) = "<" ++ show v ++ ">"
  show (Skip n) = "skip[" ++ show n ++ "]"
  show (MatrixForm _) = "mat"
  show (EitherOr p a b) = "{" ++ show p ++ ": " ++ show a ++ ", 1-" ++ show p ++ ": " ++ show b ++ "}"
  show (AndThen a b) = "<" ++ show a ++ ", " ++ show b ++ ">"
  show (Possibly p a) = "{" ++ show p ++ "? " ++ show a ++ "}"
  show (UniformDistOver as) = "{" ++ show as ++ "}"
  show (FiniteDistOver as) = "{" ++ (intercalate ", " (map (\(a, p) -> show p ++ ": " ++ show a) as)) ++ "}"
  show (GeometricRepeat p a) = show a ++ "[" ++ show p ++ "...]"
  show (FiniteDistRepeat ps a) = show a ++ "[" ++ show ps ++ "]"
  show (UniformDistRepeat n a) = show a ++ "[0.." ++ show n ++ "]"
  show (ReverseSequence a) = "reverse(" ++ show a ++ ")"
  show (Collapse _ _ n a) = "collapse(" ++ show n ++ ", " ++ show a ++ ")"
  show (SkipDist ps a) = "<skip[" ++ show ps ++ "], " ++ show a ++ ">"



type ProbSeq s = Fix (Constructor s)

data ConstructorWith s a t = ConstructorWith {
    with :: a
  , constructor :: Constructor s t
  } deriving (Show, Functor, Foldable, Traversable)

newtype ProbSeqWith s a = ProbSeqWith (Fix (ConstructorWith s a))
  deriving Show
unProbSeqWith (ProbSeqWith x) = x

instance Functor (ProbSeqWith s) where
  fmap f (ProbSeqWith (Fix constr)) = ProbSeqWith . Fix $
    constr {
      with = f (with constr)
    , constructor = (unProbSeqWith . fmap f . ProbSeqWith) <$> (constructor constr)
    }

instance Foldable (ProbSeqWith s) where
  foldMap f (ProbSeqWith (Fix constr)) = foldMap (f . with . unFix) constr

instance Traversable (ProbSeqWith s) where
  traverse f (ProbSeqWith (Fix constr)) =
    (ProbSeqWith . Fix) <$> (ConstructorWith <$> with' <*> constructor')
    where constructor' = traverse ((unProbSeqWith <$>) . traverse f . ProbSeqWith) (constructor constr)
          with' = f (with constr)

mapWith :: ((ConstructorWith s a a) -> b)
        -> ProbSeqWith s a
        -> ProbSeqWith s b
mapWith cf (ProbSeqWith (Fix constr)) =
  let constr' = constr { with = cf (fmap (with . unFix) constr) }
  in ProbSeqWith . Fix $ constr' {
      constructor = (unProbSeqWith . mapWith cf . ProbSeqWith) <$> constructor constr'
    }
