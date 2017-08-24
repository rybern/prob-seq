{-# LANGUAGE MonadComprehensions, ViewPatterns #-}
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

-- indices between sparse and data.vector are off
collapse :: (Eq a) => (Vector a -> a) -> Int -> MatSeq a -> MatSeq a
collapse concatLabels n seq = MatSeq {
    trans = trans'
  , stateLabels = V.map concatLabels stateTuples
  }
  where tuples = V.map (V.map pred) $ stateNPaths n (trans seq)
        stateTuples = V.map (V.map (stateLabels seq V.!)) tuples
        nTuples = V.length tuples

        (mainStart, mainTrans, endsStart, endsTrans) = splitTransTokens (trans seq)

        startTrans' = M.vecFromAssocList . V.toList . V.indexed . V.map (flip sequencePrefixProbability seq)
          $ stateTuples

        endsTrans' = M.fromRows . V.toList . V.map (M.row endsTrans . V.last) $ tuples

        tupleTrans ((tuples V.!) -> t1, (tuples V.!) -> t2) =
          if V.tail t1 == V.init t2
          then mainTrans M.# (V.last t1, V.last t2)
          else 0.0

        mainTrans' = buildMatrix (nTuples, nTuples) tupleTrans

        trans' = joinTransTokens (startTrans', mainTrans', endsStart, endsTrans')



stateNPaths :: Int
            -> Trans
            -> V.Vector (V.Vector Int)
stateNPaths n trans = graphNPaths n stateIxs stateOuts
  where stateIxs = V.fromList [1 .. nStates trans + 1]
        (_, mainTrans, endsStart, endsTrans) = splitTransTokens trans
        stateOuts s = V.fromList . map (pred . fst) . tail . M.vecToAssocList $ M.row mainTrans s

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
