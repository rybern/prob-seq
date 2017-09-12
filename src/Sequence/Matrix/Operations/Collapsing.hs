{-# LANGUAGE MonadComprehensions, ViewPatterns, OverloadedLists #-}
module Sequence.Matrix.Operations.Collapsing where

import Sequence.Matrix.Types
import Sequence.Matrix.Emissions
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

import Debug.Trace

{-
What should the behavior be in something like:
Collapse 5 (EitherOr 0.5 (Deterministic [1,2]) (Deterministic [1,2,3,4,5]))
maybe bad example, cant think too sleepy

but shouldn't there sometimes be the be a chance for collapsed states that were truncated to end in skips?
-}

-- is collapse . geometric infinite?

-- returns [] instead of [1] if n longer than seq
collapse :: (Eq a) => (Vector a -> a) -> Int -> MatSeq a -> MatSeq a
collapse concatLabels n seq = --traceShow (V.length (stateLabels seq), (trans seq), n, "trans'", trans', tuples) $
  MatSeq {
    trans = if M.isZeroMx trans' then M.sparseMx [[1]] else trans'
  , stateLabels = V.map (\(V.unzip -> (s, t)) -> (concatLabels s, StateTag 0 (V.toList t))) stateTuples
  }
  where (main, ends) = splitEnds (trans seq)
        squareMain = addStartColumn main
        (shiftedStartTuplePairs, endsStart') = monolith (squareMain, ends) (n + 1) 1
        startTupleMap = Map.mapKeys (V.tail . V.map (\n->n-2)) . Map.fromList . V.toList $ shiftedStartTuplePairs

        tuples = V.map (V.map pred) $ stateNPaths n (trans seq)
        stateTuples = V.map (V.map (stateLabels seq V.!)) tuples
        nTuples = V.length tuples

        (mainStart, mainTrans, endsStart, endsTrans) = splitTransTokens (trans seq)

        startTrans' = toVec . V.map (\tup -> Map.findWithDefault 0.0 tup startTupleMap) $ tuples

        f :: Vector a -> (Prob, Dist)
        f tup = undefined
        tupleProbs = V.map f tuples
        starts = fst . V.unzip $ tupleProbs

        startTrans'' = toVec starts

        endsStart'' = endsStart + (sum . V.map (\(p, ends) -> (p *) <$> ends) $ tupleProbs)

        tupleTrans ((tuples V.!) . pred -> t1, (tuples V.!) . pred -> t2) =
          if V.tail t1 == V.init t2
          then mainTrans M.# (V.last t1 + 1, V.last t2 + 1)
          else 0.0

        mainTrans' = buildMatrix (nTuples, nTuples) tupleTrans

        endsTrans' = M.fromRows . V.toList . V.map (M.row endsTrans . succ . V.last) $ tuples

        trans' = joinTransTokens (startTrans', mainTrans', endsStart', endsTrans')

-- THE INDICES FROM vecToAssocList row WILL NOT TRANSLATE UNLESS YOU ADD THE START COLUMN OR SPECIALIZE STARTS
--prepend starting vec and reduce the indices afterward
monolith :: (Trans, Trans)
         -> Int
         -> M.Index
         -> (Vector (Vector Int, Prob), Dist)
monolith _ 1 current = ([([current], 1.0)], M.zeroVec 1)
monolith (main, ends) n current = V.foldl join (V.empty, rowEnds) $ V.map recurse subtrees
  where rowEnds = M.row ends current
        subtrees = V.fromList . tail . M.vecToAssocList $ M.row main current
        recurse (ix, p) =
          let (tuples, rowEnds) = monolith (main, ends) (n-1) ix
          in (V.map (\(tuple, q) -> (current `V.cons` tuple, p * q)) tuples, (p *) <$> rowEnds)
        join (tuples1, startEnds1) (tuples2, startEnds2) = (tuples1 <> tuples2, startEnds1 + startEnds2)

stateNPaths :: Int
            -> Trans
            -> V.Vector (V.Vector Int)
stateNPaths n trans = graphNPaths n stateIxs stateOuts
  where stateIxs = V.fromList [1 .. nStates trans]
        (_, mainTrans, _, _) = splitTransTokens trans
        stateOuts s = V.fromList . map fst . tail . M.vecToAssocList $ M.row mainTrans s

graphNPaths :: Int
            -> V.Vector Int
            -> (Int -> V.Vector Int)
            -> V.Vector (V.Vector Int)
graphNPaths n stateIxs stateOuts = V.map V.fromList . V.fromList . nub $ concatMap (groups n) (V.toList stateIxs)
  where groups :: Int -> Int -> [[Int]]
        groups 1 s = [[s]]
        groups n s = concatMap (map (s:) . groups (n - 1)) (stateOuts s)




{-
        prob from to = mainTrans M.# (from + 1, to + 1)
        pathProb = fst
                   . V.foldl1 (\(p, prev) (_, next) -> (p * prob prev next, next))
                   . V.map (\s -> (1, s))

        tupleTrans :: Int -> Int -> Prob
        tupleTrans i1 i2 = prob (V.head t1) (V.head t2)
          where t1 = tuples V.! i1
                t2 = tuples V.! i2

        mainTrans' = M.fromAssocList
          [((succ i1, succ i2), tupleTrans i1 i2)
          | i1 <- [0..V.length tuples - 1]
          , i2 <- [0..V.length tuples - 1]]

        startTrans' = M.vecFromAssocList . concat $
          [ V.toList
            . V.imap (\i' tup -> (succ i', s * pathProb tup))
            . V.filter (\tup -> V.head tup + 1 == i)
            $ tuples
          | (i, s) <- tail $ M.vecToAssocList mainStart]

        endsTrans' = --M.fromRows . M.vecFromAssocList .
          concat
          [ V.toList
            . V.map (\(i', _) -> (i' + 1, r))
            . V.filter (\(i', tup) -> V.last tup + 1 == i)
            . V.zip (V.fromList [1..])
            $ tuples
          | (i, r) <- tail $ M.vecToAssocList (M.rows endsTrans)]

        endsTrans'' =
          concatMap (\(i, r) -> V.toList
                                . V.concat
                                . V.toList
                                . V.imap (\i' tup ->
                                             if V.last tup + 1 == i
                                             then V.singleton (i' + 1, r)
                                             else V.empty)
                                $ tuples)
          . tail
          . M.vecToAssocList
          . M.rows
          $ endsTrans

        endsTrans''' = foldl
          (\m (i, r) -> M.replaceRow r i m)
          (M.zeroMx (M.height mainTrans', M.width endsTrans))
          endsTrans''

        trans' = joinTransTokens (startTrans', mainTrans', endsStart, endsTrans''')
-}
