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
import qualified Math.LinearAlgebra.Sparse as M

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
  where steps = transSteps trans
        trans' = sum $ zipWith scale ps steps
