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

type ProbSeq s = Fix (Constructor s)

data ConstructorWith s a t = ConstructorWith {
    with :: a
  , constructor :: Constructor s t
  } deriving (Show, Functor, Foldable, Traversable)

-- should maybe write these for core constructors instead
instance (Show s, Show a) => Show (Fix (ConstructorWith s a)) where
  show t = cata show t

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

state :: s -> ProbSeq s
state v = Fix $ State v

skip :: Int -> ProbSeq s
skip n = Fix $ Skip n

skipDist :: [Prob] -> ProbSeq s -> ProbSeq s
skipDist ps a = Fix $ SkipDist ps a

matrixForm :: (MatSeq s) -> ProbSeq s
matrixForm m = Fix $ MatrixForm m

eitherOr :: Prob -> ProbSeq s -> ProbSeq s -> ProbSeq s
eitherOr p a b = Fix $ EitherOr p a b

andThen :: ProbSeq s -> ProbSeq s -> ProbSeq s
andThen a b = Fix $ AndThen a b

andThen' :: ProbSeq s -> ProbSeq s -> ProbSeq s
andThen' a b = Fix $ AndThen' a b

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

uniformDistRepeat' :: Int -> ProbSeq s -> ProbSeq s
uniformDistRepeat' n a = Fix $ UniformDistRepeat' n a

finiteDistRepeat :: [Prob] -> ProbSeq s -> ProbSeq s
finiteDistRepeat ps a = Fix $ FiniteDistRepeat ps a

finiteDistRepeat' :: [Prob] -> ProbSeq s -> ProbSeq s
finiteDistRepeat' ps a = Fix $ FiniteDistRepeat' ps a

series :: [ProbSeq s] -> ProbSeq s
series as = Fix $ Series as

series' :: [ProbSeq s] -> ProbSeq s
series' as = Fix $ Series' as

repeatSequence :: Int -> ProbSeq s -> ProbSeq s
repeatSequence n a = Fix $ Repeat n a

repeatSequence' :: Int -> ProbSeq s -> ProbSeq s
repeatSequence' n a = Fix $ Repeat n a
