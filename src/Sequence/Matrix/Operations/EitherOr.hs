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
--import qualified Math.LinearAlgebra.Sparse as M
import qualified SparseMatrix as M

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Sequence.Matrix.Operations.Deterministic

test1 = eitherOr 0.5 emptySequence emptySequence
test2 = eitherOr 0.5 (skip 1) emptySequence

eitherOr' p a b = (startEndsA, startEndsB, startEnds)
  where (startA, mainTransA, startEndsA, endTransA) = splitTransTokens $ trans a
        (startB, mainTransB, startEndsB, endTransB) = splitTransTokens $ trans b

        startEnds = (p `M.scaleV` startEndsA) `M.addVec` ((1 - p) `M.scaleV` startEndsB)


eitherOr :: Prob -> MatSeq s -> MatSeq s -> MatSeq s
eitherOr p a b = MatSeq {
    trans = joinTransTokens (start, mainTrans, startEnds, ends)
  , stateLabels = stateLabels'
  }
  where stateLabels' = appendLabel 0 (stateLabels a) <> appendLabel 1 (stateLabels b)

        (startA, mainTransA, startEndsA, endTransA) = splitTransTokens $ trans a
        (startB, mainTransB, startEndsB, endTransB) = splitTransTokens $ trans b

        start = (p `M.scaleV` startA) <> ((1 - p) `M.scaleV` startB)

        startEnds = (p `M.scaleV` startEndsA) `M.addVec` ((1 - p) `M.scaleV` startEndsB)

        endLen = max (M.width endTransA) (M.width endTransB)
        ends = M.vconcat [setWidth endLen endTransA, setWidth endLen endTransB]

        mainTrans = mainTransA `diagConcat` mainTransB

possibly :: Prob -> MatSeq s -> MatSeq s
possibly p = eitherOr (1-p) emptySequence
