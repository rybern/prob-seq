{-# LANGUAGE OverloadedLists, RecordWildCards #-}
module Sequence.Matrix.Operations.SkipDist where

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

import Sequence.Matrix.Operations.AndThen

skipDist :: [Prob] -> (MatSeq s) -> (MatSeq s)
skipDist ps (MatSeq {..}) = MatSeq {
    stateLabels = appendLabel 0 stateLabels
  , trans = trans'
  }
  where steps = stationary (nStates trans) : transSteps trans
        -- normalize the initial row, because the stationary distribution has 0 in the first row
        -- could also scale first row by 1 / sum (tail ps)
        trans' = mapRow 1 normalizeVec . sum $ zipWith M.scale ps steps
