{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
module Sequence.ProbTree where

import Data.Maybe (catMaybes)
import Data.List
import Control.Monad.Random
import Control.Monad.Loops
import Sequence.Types
import qualified Data.Vector as V
import GHC.Exts (sortWith, groupWith)

data ProbTree s = DistOver [(ProbTree s, Prob)]
                | Series [ProbTree s]
                | Det s
                deriving (Functor, Show)

sampleProbTreeWith :: (Monoid s, MonadRandom m)
                   => ([(ProbTree s, Prob)] -> m (ProbTree s))
                   -> ProbTree s -> m s
sampleProbTreeWith sample (DistOver branches) = sample branches >>= sampleProbTree
sampleProbTreeWith _ (Series seqs) = mconcat <$> mapM sampleProbTree seqs
sampleProbTreeWith _ (Det seq) = return seq

sampleProbTree :: (Monoid s, MonadRandom m) => ProbTree s -> m s
sampleProbTree = sampleProbTreeWith fromList

sampleProbTreeUniform :: (Monoid s, MonadRandom m) => ProbTree s -> m s
sampleProbTreeUniform = sampleProbTreeWith (fromList . toUniform)
  where toUniform xs =
          let uniform = 1 / (fromIntegral (length xs))
          in map (\(a, _) -> (a, uniform)) xs

matchPrefix :: (a -> a -> Maybe a)
            -> a
            -> ProbTree a
            -> [(a, Prob)]
matchPrefix takePrefix query (DistOver branches) =
  concatMap (\(subtree, pb) -> (\(a, p) -> (a, pb * p)) <$> matchPrefix takePrefix query subtree) $ branches
matchPrefix takePrefix query (Series trees) = concatM treeMatches (query, 1.0)
  where nextMatches tree (query, p) = map (\(a, p') -> (a, p*p')) $ matchPrefix takePrefix query tree
        treeMatches = map nextMatches trees
matchPrefix takePrefix query (Det seq') =
  maybe [] (\rest -> [(rest, 1.0)]) $ takePrefix query seq'

consolidateMatches :: (Ord a) => [(a, Prob)] -> [(a, Prob)]
consolidateMatches = map ((\(a:_, ps) -> (a, sum ps)) . unzip) . groupWith fst . sortWith fst

probOf :: (a -> a -> Maybe a)
        -> (a -> Bool)
        -> a
        -> ProbTree a
        -> Prob
probOf prefix isEmpty query tree =
  sum . map snd . filter (isEmpty . fst) $ matchPrefix prefix query tree

probOfVec :: (Eq s) => V.Vector s -> ProbTree (V.Vector s) -> Prob
probOfVec = probOf vecTakePrefix null

vecTakePrefix :: (Eq s) => V.Vector s -> V.Vector s -> Maybe (V.Vector s)
vecTakePrefix query seq = do
  let lenQuery = V.length query
      lenSeq = V.length seq
  guard $ lenQuery >= lenSeq
  guard $ V.and $ V.zipWith (==) query seq
  if lenQuery == lenSeq
    then Just V.empty
    else Just $ V.drop lenSeq query
