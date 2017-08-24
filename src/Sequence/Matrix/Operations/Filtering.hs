{-# LANGUAGE RecordWildCards #-}
module Sequence.Matrix.Operations.Filtering where

import Sequence.Matrix.Types
import Sequence.Matrix.SparseMatrixUtils
import Sequence.Matrix.ProbSeqMatrixUtils
import Data.Monoid ((<>))
import Data.List (find, nub)
import Data.Maybe (fromJust, fromMaybe)
import Control.Applicative (liftA2)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Math.LinearAlgebra.Sparse as M

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

{-
could reduce the graph by looking for identical state sequences to be collapsed
identical sequences reduces to subgraph isomorphism problem, which is NP hard

for now, collapse identical states by identical rows, summing cols

when are there ever identical states?
-}


filterStates :: Set M.Index -> MatSeq s -> MatSeq s
filterStates reachable (MatSeq {..}) = MatSeq {
    stateLabels = stateLabels'
  , trans = trans'
  }
  where trans' = filterRows keepRows . filterCols keepCols $ trans
        stateLabels' = V.ifilter (\ix _ -> succ ix `Set.member` reachable) stateLabels
        keepRows = Set.insert 1 . Set.fromList . map succ . Set.toList $ reachable
        keepCols = Set.fromList [nStates trans + 1 .. M.width trans] `Set.union` reachable

filterUnreachableStates :: MatSeq s -> MatSeq s
filterUnreachableStates s = filterStates (transReachable (trans s)) s

transReachable :: Trans -> Set M.Index
transReachable = uncurry reachable . transGraph

transGraph :: Trans -> (Set M.Index, Map M.Index (Set M.Index))
transGraph t = ( fromMaybe Set.empty (0 `Map.lookup` dists)
               , 0 `Map.delete` dists)
  where dists = Map.fromList . map (\(ix, dist) -> (ix - 1, rowSet dist)) . tail . M.vecToAssocList . M.rows $ t

rowSet :: Dist -> Set M.Index
rowSet = Set.fromList . map fst . filter ((> 0) . snd) . tail . M.vecToAssocList

reachable :: (Ord a) => (Set a) -> Map a (Set a) -> Set a
reachable initialReachable reachMap = Set.intersection keyset . snd . fromJust $ find (null . fst) steps
  where steps = iterate step (Set.toList initialReachable, initialReachable)
        step ((e:es), known) = case e `Map.lookup` reachMap of
                                 Nothing -> (es, known)
                                 Just found -> let new = found `Set.difference` known
                                               in (es <> Set.toList new, known <> new)
        keyset = Map.keysSet reachMap
