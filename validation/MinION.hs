{-# LANGUAGE FlexibleInstances, DeriveFoldable, DeriveFunctor #-}
module MinION where

import Sequence
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List
import Data.Foldable

class Joinable a where
  join :: (Foldable f) => f a -> a

data StateTree a = Collapsed [StateTree a]
                 | Alone a
                 deriving (Eq, Show, Functor, Foldable)

instance Joinable (StateTree a) where
  join = Collapsed . toList

instance Joinable String where
  join = (\s -> "(" ++ s ++ ")") . intercalate "," . toList

keyOrder :: [Char]
keyOrder = ['A', 'C', 'G', 'T']

joinState :: Vector (StateTree a) -> StateTree a
joinState = Collapsed . V.toList

ntTreeToString :: StateTree Char -> String
ntTreeToString (Collapsed sts) = (\s -> "(" ++ s ++ ")") . intercalate "," . map ntTreeToString $ sts
ntTreeToString (Alone a) = [a]

minion :: (Joinable a) => ProbSeq a -> ProbSeq a
minion = skipDist [0.4, 0.25, 0.15, 0.1, 0.1]
       . collapse undefined join 5
