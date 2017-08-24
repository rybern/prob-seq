module Sequence.Matrix.Operations.Reverse where

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

reverseSequence :: MatSeq s -> MatSeq s
reverseSequence s = s {
    trans = trans'
  }
  where squareTrans = collapseEnds $ trans s
        (mainStart, mainTrans, endsStart, endsTrans) = splitTransTokens squareTrans
        mainStart' = M.col endsTrans 1
        mainTrans' = M.trans mainTrans
        endsTrans' = fromCols [mainStart]

        trans' = joinTransTokens (mainStart', mainTrans', endsStart, endsTrans')
