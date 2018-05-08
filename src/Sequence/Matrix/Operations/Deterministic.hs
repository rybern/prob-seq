{-# LANGUAGE OverloadedLists #-}
module Sequence.Matrix.Operations.Deterministic where

import Sequence.Matrix.Types
import Sequence.Matrix.SparseMatrixUtils
import Sequence.Matrix.ProbSeqMatrixUtils
import Data.Monoid ((<>))
import Data.List (find, nub)
import Data.Maybe (fromJust, fromMaybe)
import Control.Applicative (liftA2)
import Data.Vector (Vector)
import qualified Data.Vector as V
--import qualified Math.LinearAlgebra.Sparse as M
import qualified SparseMatrix as M

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map



emptySequence :: MatSeq s
emptySequence = MatSeq {
    trans = M.fromRows (V.singleton (M.singVec 1))
  , stateLabels = V.empty
  }

skip :: Int -> MatSeq s
skip n = MatSeq {
    trans = M.fromRows (V.singleton (onehotVector (succ n) (succ n)))
  , stateLabels = V.empty
  }

state :: s -> MatSeq s
state s = MatSeq {
    trans = M.idMx 2
  , stateLabels = V.singleton (s, StateTag 0 [])
  }
