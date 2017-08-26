module Sequence.Matrix.Operations.Insertion where

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

import Sequence.Matrix.Operations.AndThen

-- WOULD TESTING BE WAY EASIER AFTER IMPLEMENTING STATE TAGS?


-- what if the state points to itself, and the delim has skips? it could loop to itself, like geometricRepeat
  -- One cheap way to avoid this is to use the original distribution for looping back - but it's not pretty. That would mean skip 0 ends can then restart the delim, but skip 1 couldn't, so skip doesn't behave as normally defined
-- to summarize: a dist that skips won't work as expected with a stationary state

-- the 'end' of the delim represents returning control to the original. does that mean you can never insert a chance of ending the sequence? how limiting is that?

-- test by adding a specific test case for inserting at various points to make it easier.
-- examples:
  -- insert a 0 b = andThen a b
  -- insert a (length a) b = andThen b a
  -- do an insert where the label sets of seq and delim are different, so that it's clear where the split is (don't have to worry about empty delim, it's all the same probability)
insert :: MatSeq s -> Int -> MatSeq s -> MatSeq s
insert seq 0 delim = delim `andThen` seq
insert seq ix delim =
  if ix < 0 || ix > V.length (stateLabels seq)
  then seq
  else MatSeq {
    trans = trans'
  , stateLabels = stateLabels'
  }
  where stateLabels' = stateLabels seq <> stateLabels delim
        nStates = V.length (stateLabels seq)
        nStates' = V.length stateLabels'
        nNewStates = V.length (stateLabels delim)

        (delimMainStart, delimMainTrans, delimEndsStart, delimEndsTrans) = splitTransTokens (trans delim)
        (seqMain, seqEnds) = splitEnds (trans seq)
        stateRowMain = M.row seqMain (ix + 1)
        expSeqMain = M.hconcat [seqMain, M.zeroMx (nStates, nNewStates)]

        -- weighted sum of convolving delimEnds with each target, in unexpanded seq
        -- how to deal with ends?
        translateDelimEnds = distributeEndDistAtState (trans seq) ix

        stateProbNotEnd = (1 -) . sum $ M.row seqEnds (ix + 1)
        stateRowMain' = translateDelimEnds delimEndsStart <> ((stateProbNotEnd *) <$> delimMainStart)

        expSeqMain' = M.replaceRow stateRowMain' (ix + 1) expSeqMain

        delimLeft = mapRows translateDelimEnds delimEndsTrans

        seqMain' = M.vconcat [ expSeqMain'
                             , M.hconcat [delimLeft, delimMainTrans]]
        seqEnds' = M.vconcat [ seqEnds
                             , M.zeroMx (fst $ M.dims seqEnds, nNewStates)]
        trans' = M.hconcat [seqMain', seqEnds']

distributeEndDistAtState :: Trans -> Int -> Dist -> Dist
distributeEndDistAtState trans ix end = distributeEndDist trans' end
  where trans' = M.replaceRow (M.row trans (ix + 1)) 1 trans


{-
(delete, insert, mutate, slips, etc) -> intersperse -> insertAfterState :: MatSeq -> MatSeq -> Int -> MatSeq
-}

