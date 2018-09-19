{-# LANGUAGE TupleSections, RecordWildCards, DeriveTraversable, DeriveFunctor, ViewPatterns, FlexibleInstances #-}
module Sequence.Constructors where

import Sequence.Matrix.Types
import Data.Vector (Vector)
import Data.Fix
import Data.List
import qualified Data.Vector as V

data Constructor s t =
    EmptySequence
  | State s
  | Skip Int
  | SkipDist [Prob] t
  | MatrixForm (MatSeq s)
  | EitherOr Prob t t
  | AndThen t t
  | AndThen' t t
  -- p probability of STAYING in t
  | GeometricRepeat Prob t
  | ReverseSequence t
  | Collapse (s -> Vector s) (Vector s -> s) Int t

  | Possibly Prob t
  | UniformDistOver [t]
  | FiniteDistOver [(t, Prob)]
  | FiniteDistRepeat [Prob] t
  | FiniteDistRepeat' [Prob] t
  | UniformDistRepeat Int t
  | UniformDistRepeat' Int t
  | Series [t]
  | Series' [t]
  | Repeat Int t
  | Repeat' Int t
  deriving (Functor, Foldable, Traversable)

showL :: (a -> String) -> [a] -> String
showL s xs = "[" ++ intercalate "," (map s xs) ++ "]"

showConstructor :: (Show s) => Constructor s String -> String
showConstructor EmptySequence = "emptySequence"
showConstructor (State s) = "state (" ++ show s ++ ")"
showConstructor (Skip n) = "skip " ++ show n
showConstructor (SkipDist ps s) = "skipDist " ++ show ps ++ " (" ++ s ++ ")"
showConstructor (MatrixForm _) = "mat"
showConstructor (EitherOr p a b) = "eitherOr " ++ show p ++ " (" ++ a ++ ") (" ++ b ++ ")"
showConstructor (AndThen a b) = "andThen (" ++ a ++ ") (" ++ b ++ ")"
showConstructor (AndThen' a b) = "andThen' (" ++ a ++ ") (" ++ b ++ ")"
showConstructor (Possibly p a) = "possibly " ++ show p ++ " (" ++ a ++ ")"
showConstructor (UniformDistOver as) = "uniformDistOver " ++ showL id as
showConstructor (FiniteDistOver as) = "finiteDistOver " ++ showL (\(s, p) -> "(" ++ s ++ ", " ++ show p ++ ")") as
showConstructor (GeometricRepeat p a) = "geometricRepeat " ++ show p ++ " (" ++ a ++ ")"
showConstructor (FiniteDistRepeat ps a) = "finiteDistRepeat " ++ show ps ++ " (" ++ a ++ ")"
showConstructor (UniformDistRepeat n a) = "uniformDistRepeat " ++ show n ++ " (" ++ a ++ ")"
showConstructor (ReverseSequence a) = "reverseSequence (" ++ a ++ ")"
showConstructor (Collapse _ _ n a) = "collapse treeToVec vecToTree " ++ show n ++ " (" ++ a ++ ")"
showConstructor (Series as) = "series " ++ showL id as
showConstructor (Series' as) = "series' " ++ showL id as
showConstructor (Repeat n a) = "repeat " ++ show n ++ " (" ++ a ++ ")"

instance (Show s, Show t) => Show (Constructor s t) where
  show = showConstructor . fmap show

-- should maybe write these for core constructors instead
instance (Show s) => Show (Fix (Constructor s)) where
  show t = cata showConstructor t

type ProbSeq s = ProbSeqWith s (Maybe Int)
--type ProbSeq s = Fix (Constructor s)

data ConstructorWith s a t = ConstructorWith {
    with :: a
  , constructor :: Constructor s t
  } deriving (Show, Functor, Foldable, Traversable)

-- should maybe write these for core constructors instead
instance (Show s, Show a) => Show (Fix (ConstructorWith s a)) where
  show t = cata show t

newtype ProbSeqWith s a = ProbSeqWith { unProbSeqWith :: Fix (ConstructorWith s a) }
  deriving Show

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

untagged :: Constructor s (ProbSeq s) -> ProbSeq s
untagged c = ProbSeqWith . Fix $ ConstructorWith Nothing (unProbSeqWith <$> c)

emptySequence :: ProbSeq s
emptySequence = untagged EmptySequence

state :: s -> ProbSeq s
state v = untagged $ State v

skip :: Int -> ProbSeq s
skip n = untagged $ Skip n

skipDist :: [Prob] -> ProbSeq s -> ProbSeq s
skipDist ps a = untagged $ SkipDist ps a

matrixForm :: (MatSeq s) -> ProbSeq s
matrixForm m = untagged $ MatrixForm m

eitherOr :: Prob -> ProbSeq s -> ProbSeq s -> ProbSeq s
eitherOr p a b = untagged $ EitherOr p a b

andThen :: ProbSeq s -> ProbSeq s -> ProbSeq s
andThen a b = untagged $ AndThen a b

andThen' :: ProbSeq s -> ProbSeq s -> ProbSeq s
andThen' a b = untagged $ AndThen' a b

geometricRepeat :: Prob -> ProbSeq s -> ProbSeq s
geometricRepeat p a = untagged $ GeometricRepeat p a

reverseSequence :: ProbSeq s -> ProbSeq s
reverseSequence a = untagged $ ReverseSequence a

collapse :: (s -> Vector s) -> (Vector s -> s) -> Int -> ProbSeq s -> ProbSeq s
collapse f g n a = untagged $ Collapse f g n a

possibly :: Prob -> ProbSeq s -> ProbSeq s
possibly p a = untagged $ Possibly p a

uniformDistOver :: [ProbSeq s] -> ProbSeq s
uniformDistOver as = untagged $ UniformDistOver as

finiteDistOver :: [(ProbSeq s, Prob)] -> ProbSeq s
finiteDistOver pairs = untagged $ FiniteDistOver pairs

uniformDistRepeat :: Int -> ProbSeq s -> ProbSeq s
uniformDistRepeat n a = untagged $ UniformDistRepeat n a

uniformDistRepeat' :: Int -> ProbSeq s -> ProbSeq s
uniformDistRepeat' n a = untagged $ UniformDistRepeat' n a

finiteDistRepeat :: [Prob] -> ProbSeq s -> ProbSeq s
finiteDistRepeat ps a = untagged $ FiniteDistRepeat ps a

finiteDistRepeat' :: [Prob] -> ProbSeq s -> ProbSeq s
finiteDistRepeat' ps a = untagged $ FiniteDistRepeat' ps a

series :: [ProbSeq s] -> ProbSeq s
series as = untagged $ Series as

series' :: [ProbSeq s] -> ProbSeq s
series' as = untagged $ Series' as

repeatSequence :: Int -> ProbSeq s -> ProbSeq s
repeatSequence n a = untagged $ Repeat n a

repeatSequence' :: Int -> ProbSeq s -> ProbSeq s
repeatSequence' n a = untagged $ Repeat n a
