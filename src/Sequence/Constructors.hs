{-# LANGUAGE TupleSections, RecordWildCards, DeriveTraversable, DeriveFunctor, ViewPatterns, FlexibleInstances, OverloadedLists #-}
module Sequence.Constructors where

import Sequence.Matrix.Types
import Data.Vector (Vector)
import Data.Fix
import Data.List
import qualified Data.Vector as V

import Sequence.Tags.Utils
import Control.Monad.State hiding (state)

data Constructor s t =
    EmptySequence
  | State s
  | Skip Int
  | SkipDist [Prob] t
  | MatrixForm (MatSeq s)
  | EitherOr (Maybe Int) Prob t t
  | AndThen t t
  | AndThen' t t
  -- p probability of STAYING in t
  | GeometricRepeat Prob t
  | ReverseSequence t
  | Collapse (s -> Vector s) (Vector s -> s) Int t

  | Possibly (Maybe Int) Prob t
  | UniformDistOver (Maybe Int) [t]
  | FiniteDistOver (Maybe Int) [(t, Prob)]
  | FiniteDistRepeat (Maybe Int) [Prob] t
  | FiniteDistRepeat' (Maybe Int) [Prob] t
  | UniformDistRepeat (Maybe Int) Int t
  | UniformDistRepeat' (Maybe Int) Int t
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
showConstructor (EitherOr t p a b) = "eitherOr " ++ show p ++ " (" ++ a ++ ") (" ++ b ++ ")"
showConstructor (AndThen a b) = "andThen (" ++ a ++ ") (" ++ b ++ ")"
showConstructor (AndThen' a b) = "andThen' (" ++ a ++ ") (" ++ b ++ ")"
showConstructor (Possibly t p a) = "possibly " ++ show p ++ " (" ++ a ++ ")"
showConstructor (UniformDistOver t as) = "uniformDistOver " ++ showL id as
showConstructor (FiniteDistOver t as) = "finiteDistOver " ++ showL (\(s, p) -> "(" ++ s ++ ", " ++ show p ++ ")") as
showConstructor (GeometricRepeat p a) = "geometricRepeat " ++ show p ++ " (" ++ a ++ ")"
showConstructor (FiniteDistRepeat t ps a) = "finiteDistRepeat " ++ show ps ++ " (" ++ a ++ ")"
showConstructor (UniformDistRepeat t n a) = "uniformDistRepeat " ++ show n ++ " (" ++ a ++ ")"
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

states :: [s] -> ProbSeq s
states = series . map state

skip :: Int -> ProbSeq s
skip n = Fix $ Skip n

skipDist :: [Prob] -> ProbSeq s -> ProbSeq s
skipDist ps a = Fix $ SkipDist ps a

matrixForm :: (MatSeq s) -> ProbSeq s
matrixForm m = Fix $ MatrixForm m

eitherOrM :: Prob -> ProbSeq s -> ProbSeq s -> State TagGen (ProbSeq s, Tag Bool)
eitherOrM p a b = do
  t <- newTag [True, False]
  return (Fix $ EitherOr (Just (tagId t)) p a b, t)

eitherOr :: Prob -> ProbSeq s -> ProbSeq s -> ProbSeq s
eitherOr p a b = Fix $ EitherOr Nothing p a b

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
possibly p a = Fix $ Possibly Nothing p a

uniformDistOverM :: [ProbSeq s] -> State TagGen (ProbSeq s, Tag Int)
uniformDistOverM as = do
  t <- newTag [0..length as - 1]
  return (Fix $ UniformDistOver (Just (tagId t)) as, t)

uniformDistOver :: [ProbSeq s] -> ProbSeq s
uniformDistOver as = Fix $ UniformDistOver Nothing as

finiteDistOverM :: [(ProbSeq s, Prob)] -> State TagGen (ProbSeq s, Tag Int)
finiteDistOverM pairs = do
  t <- newTag [0..length pairs - 1]
  return (Fix $ FiniteDistOver (Just (tagId t)) pairs, t)

finiteDistOver :: [(ProbSeq s, Prob)] -> ProbSeq s
finiteDistOver pairs = Fix $ FiniteDistOver Nothing pairs

uniformDistRepeatM :: Int -> ProbSeq s -> State TagGen (ProbSeq s, Tag Int)
uniformDistRepeatM n a = do
  t <- newTag [0..n-1]
  return (Fix $ UniformDistRepeat (Just (tagId t)) n a, t)

uniformDistRepeat :: Int -> ProbSeq s -> ProbSeq s
uniformDistRepeat n a = Fix $ UniformDistRepeat Nothing n a

uniformDistRepeat' :: Int -> ProbSeq s -> ProbSeq s
uniformDistRepeat' n a = Fix $ UniformDistRepeat' Nothing n a

finiteDistRepeatM :: [Prob] -> ProbSeq s -> State TagGen (ProbSeq s, Tag Int)
finiteDistRepeatM ps a = do
  t <- newTag [0..length ps - 1]
  return (Fix $ FiniteDistRepeat (Just (tagId t)) ps a, t)

finiteDistRepeat :: [Prob] -> ProbSeq s -> ProbSeq s
finiteDistRepeat ps a = Fix $ FiniteDistRepeat Nothing ps a

finiteDistRepeat' :: [Prob] -> ProbSeq s -> ProbSeq s
finiteDistRepeat' ps a = Fix $ FiniteDistRepeat' Nothing ps a

series :: [ProbSeq s] -> ProbSeq s
series as = Fix $ Series as

series' :: [ProbSeq s] -> ProbSeq s
series' as = Fix $ Series' as

repeatSequence :: Int -> ProbSeq s -> ProbSeq s
repeatSequence n a = Fix $ Repeat n a

repeatSequence' :: Int -> ProbSeq s -> ProbSeq s
repeatSequence' n a = Fix $ Repeat n a
