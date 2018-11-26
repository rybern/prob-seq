module SMoL.Matrix.Operations.Reverse where

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
import SMoL.Matrix.Operations.EitherOr

test1 = reverseSequence emptySequence

reverseSequence' t = joinTransTokens (mainStart', mainTrans', endsStart, endsTrans')
  where squareTrans = collapseEnds $ t
        (mainStart, mainTrans, endsStart, endsTrans) = splitTransTokens squareTrans
        mainStart' = M.col endsTrans 1
        mainTrans' = M.trans mainTrans
        endsTrans' = M.fromCols [mainStart]

reverseSequence :: MatSeq s -> MatSeq s
reverseSequence s = s {
    trans = normalizeMat $ joinTransTokens (mainStart', mainTrans', endsStart, endsTrans')
  , stateLabels = appendLabel 0 $ stateLabels s
  }
  where squareTrans = collapseEnds $ trans s
        (mainStart, mainTrans, endsStart, endsTrans) = splitTransTokens squareTrans
        mainStart' = M.col endsTrans 1
        mainTrans' = M.trans mainTrans
        endsTrans' = M.fromCols [mainStart]
