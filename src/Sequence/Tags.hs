{-# LANGUAGE DeriveFunctor #-}
module Sequence.Tags where

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
import Sequence

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

type TagIxs = Map TagID (Vector IntSet)

data Emissions d = Emissions -- this should be a Matrix Prob and s <-> Index
data Posterior = Posterior { unposterior :: IntMap Prob }

subsetIxs :: IntSet -> Posterior -> Posterior
subsetIxs ixs (Posterior ems) = Posterior $ IntMap.restrictKeys ems ixs

unionPosts :: [Posterior] -> Posterior
unionPosts = Posterior . IntMap.unions . fmap unposterior

tagDist :: (Ord t) => Tag t -> Query (Map t (Posterior))
tagDist (Tag { tagId = tagId, values = values }) = do
  (post, ixs) <- ask
  let ixsVec = case Map.lookup tagId ixs of
        Nothing -> error "Using a tag that isn't in MatSeq"
        Just (ixsVec) -> ixsVec
      postMap = Map.fromList . V.toList . V.zip values . V.map (flip subsetIxs post) $ ixsVec
  return postMap

type Query = Reader (Posterior, TagIxs)

buildQuery :: ProbSeq d -> Query a -> Emissions d -> a
buildQuery ps q emis = runReader q (post, ixs)
  where (ms, ixs) = buildMatSeq' ps
        post = posterior ms emis

observe :: Emissions d -> State TagGen (Emissions d -> a) -> a
observe ems s = (runTagGen s) ems

partitionPost :: (Ord c) => Tag c -> (c -> Bool) -> Query (Posterior, Posterior)
partitionPost t pred = do
  dist <- tagDist t
  let (true, false) = Map.partitionWithKey (\k _ -> pred k) dist
      join = unionPosts . Map.elems
  return (join true, join false)

event1 :: (Ord c) => Tag c -> (c -> Bool) -> Query Prob
event1 t pred = do
  (truePost, falsePost) <- partitionPost t pred
  let add = sum . unposterior
      trueSum = add truePost
      falseSum = add falsePost
  return $ trueSum / (falseSum + trueSum)

condition1 :: (Ord c) => Tag c -> (c -> Bool) -> Query a -> Query a
condition1 t pred next = do
  (post, _) <- partitionPost t pred
  (_, ixs) <- ask
  local (const (post, ixs)) next

example :: Emissions Char -> (Prob, Prob)
example ems = observe ems $ do
  (ps1, a) <- eitherOr' 0.4 (state 'a') (state 'b')
  (ps2, b) <- eitherOr' 0.5 ps1 (state 'c')
  let ps = andThen ps2 (state 'd')
  return . buildQuery ps $ do
    anb <- condition1 b id $ event1 a not
    na <- event1 b not
    return (anb, na)

-- Implement elsewhere, using placeholders

-- TODO SHMM integration should definitely be in SMoL not SMoL-Minion
posterior :: MatSeq s -> Emissions s -> Posterior
posterior ms ems = Posterior IntMap.empty

eitherOr' :: Prob -> ProbSeq s -> ProbSeq s -> State TagGen (ProbSeq s, Tag Bool)
eitherOr' = undefined

finiteDistOver' :: [(ProbSeq s, Prob)] -> State TagGen (ProbSeq s, Tag Int)
finiteDistOver' = undefined

buildMatSeq' :: ProbSeq d -> (MatSeq d, TagIxs)
buildMatSeq' = undefined
