module Sequence.Matrix.Operations.EitherOr where

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

import Sequence.Matrix.Operations.Deterministic

eitherOr :: Prob -> MatSeq s -> MatSeq s -> MatSeq s
eitherOr p a b = MatSeq {
    trans = joinTransTokens (start, mainTrans, startEnds, ends)
  , stateLabels = stateLabels'
  }
  where stateLabels' = stateLabels a <> stateLabels b

        (startA, mainTransA, startEndsA, endTransA) = splitTransTokens $ trans a
        (startB, mainTransB, startEndsB, endTransB) = splitTransTokens $ trans b

        start = ((* p) <$> startA) <> ((* (1 - p)) <$> startB)

        startEnds = ((* p) <$> startEndsA) + ((* (1 - p)) <$> startEndsB)

        endLen = max (M.width endTransA) (M.width endTransB)
        ends = M.vconcat [setWidth endLen endTransA, setWidth endLen endTransB]

        mainTrans = mainTransA `diagConcat` mainTransB

possibly :: Prob -> MatSeq s -> MatSeq s
possibly p = eitherOr (1-p) emptySequence