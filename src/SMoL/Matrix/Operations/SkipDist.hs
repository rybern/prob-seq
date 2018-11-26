{-# LANGUAGE OverloadedLists, RecordWildCards #-}
module SMoL.Matrix.Operations.SkipDist where

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

import SMoL.Matrix.Operations.Deterministic
import SMoL.Matrix.Operations.AndThen

skipDist' ps (MatSeq {..}) = trans'
  where steps = stationary (nStates trans) : transSteps trans
        trans' = mapRow 1 normalizeVec . foldl1 M.addMx $ zipWith M.scale ps steps

skipDist :: [Prob] -> (MatSeq s) -> (MatSeq s)
skipDist ps (MatSeq {..}) = MatSeq {
    stateLabels = appendLabel 0 stateLabels
  , trans = trans'
  }
  where steps = stationary (nStates trans) : transSteps trans
        -- normalize the initial row, because the stationary distribution has 0 in the first row
        -- could also scale first row by 1 / sum (tail ps)
        trans' = mapRow 1 normalizeVec . sum $ zipWith M.scale ps steps
