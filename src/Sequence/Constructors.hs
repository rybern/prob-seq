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
  | GeometricRepeat Prob t
  | ReverseSequence t
  | Collapse (s -> Vector s) (Vector s -> s) Int t

  | Possibly Prob t
  | UniformDistOver [t]
  | FiniteDistOver [(t, Prob)]
  | FiniteDistRepeat [Prob] t
  | UniformDistRepeat Int t
  | Series [t]
  | Repeat Int t
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
  show (Series as) = "<" ++ intercalate ", " (map show as) ++ ">"
  show (Repeat n a) = "<" ++ show a ++ "[" ++ show n ++ "]" ++ ">"

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
emptySequence :: ProbSeq s
emptySequence = Fix $ EmptySequence

deterministicSequence :: (Vector s) -> ProbSeq s
deterministicSequence v = Fix $ DeterministicSequence v

skip :: Int -> ProbSeq s
skip n = Fix $ Skip n

matrixForm :: (MatSeq s) -> ProbSeq s
matrixForm m = Fix $ MatrixForm m

eitherOr :: Prob -> ProbSeq s -> ProbSeq s -> ProbSeq s
eitherOr p a b = Fix $ EitherOr p a b

andThen :: ProbSeq s -> ProbSeq s -> ProbSeq s
andThen a b = Fix $ AndThen a b

geometricRepeat :: Prob -> ProbSeq s -> ProbSeq s
geometricRepeat p a = Fix $ GeometricRepeat p a

reverseSequence :: ProbSeq s -> ProbSeq s
reverseSequence a = Fix $ ReverseSequence a

collapse :: (s -> Vector s) -> (Vector s -> s) -> Int -> ProbSeq s -> ProbSeq s
collapse f g n a = Fix $ Collapse f g n a

possibly :: Prob -> ProbSeq s -> ProbSeq s
possibly p a = Fix $ Possibly p a

uniformDistOver :: [ProbSeq s] -> ProbSeq s
uniformDistOver as = Fix $ UniformDistOver as

finiteDistOver :: [(ProbSeq s, Prob)] -> ProbSeq s
finiteDistOver pairs = Fix $ FiniteDistOver pairs

uniformDistRepeat :: Int -> ProbSeq s -> ProbSeq s
uniformDistRepeat n a = Fix $ UniformDistRepeat n a

finiteDistRepeat :: [Prob] -> ProbSeq s -> ProbSeq s
finiteDistRepeat ps a = Fix $ FiniteDistRepeat ps a

series :: [ProbSeq s] -> ProbSeq s
series as = Fix $ Series as

repeatSequence :: Int -> ProbSeq s -> ProbSeq s
repeatSequence n a = Fix $ Repeat n a
