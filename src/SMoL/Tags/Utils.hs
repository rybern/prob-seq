{-# LANGUAGE DeriveFunctor #-}
module SMoL.Tags.Utils where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad.State hiding (state)
import Control.Monad.Reader


type TagID = Int
newtype TagGen = TagGen Int

data Tag c = Tag {
    tagId :: TagID
  , values :: Vector c
  } deriving (Show, Functor)

initTagGen :: TagGen
initTagGen = TagGen 0

runTagGen :: State TagGen a -> a
runTagGen = flip evalState initTagGen

newTagID :: State TagGen TagID
newTagID = do
  (TagGen i) <- get
  put $ TagGen (i + 1)
  return i

newTag :: Vector c -> State TagGen (Tag c)
newTag values = (\tid -> Tag tid values) <$> newTagID
