{-# LANGUAGE OverloadedLists #-}
module SMoL.Matrix.Operations.Deterministic where

import SMoL.Matrix.Types
import SMoL.Matrix.SparseMatrixUtils
import SMoL.Matrix.ProbSeqMatrixUtils
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
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap



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
  , stateLabels = V.singleton (StateLabel s (StateTag 0 []) (IntMap.empty))
  }
