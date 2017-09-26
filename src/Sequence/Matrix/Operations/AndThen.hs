module Sequence.Matrix.Operations.AndThen where

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

type SkipAdd = Bool

andThen = andThenToggle True
andThen' = andThenToggle False

andThenToggle :: SkipAdd -> MatSeq s -> MatSeq s -> MatSeq s
andThenToggle skipAdd seqA seqB = MatSeq {
    trans = trans'
  , stateLabels = stateLabels'
  }
  where stateLabels' = appendLabel 0 (stateLabels seqA) <> appendLabel 1 (stateLabels seqB)

        transA = trans seqA
        transB = trans seqB
        (mainA, endsA) = splitEnds transA
        (_, nonstartB) = splitStart transB
        transTo = if skipAdd then transB else collapseEnds transB
        transition = transTo `distributeEnds` endsA -- collapseEnds transB?
        rightLen = max (M.width transition) (M.width transB)
        lowerLeft = M.zeroMx (M.height nonstartB, M.width mainA)
        trans' = M.blockMx [ [mainA, setWidth rightLen transition]
                           , [lowerLeft, setWidth rightLen nonstartB] ]
        --trans' = M.vconcat [ mainA `happend` setWidth rightLen transition
                           --, lowerLeft `happend` setWidth rightLen nonstartB ]

test' = skip 1 `andThen` emptySequence
test = trans emptySequence `distributeEnds` trans (skip 1)

happend :: (Num s, Eq s) => M.SparseMatrix s -> M.SparseMatrix s -> M.SparseMatrix s
happend a b = M.fromRows $ M.unionVecsWith mappend (M.rows a) (M.rows b)

instance Monoid (MatSeq a) where
  mappend a b = a `andThen` b
  mempty = emptySequence

{- STATE STEPPING -}

distributeEnds :: Trans -> Trans -> Trans
distributeEnds transTo transFrom = trimZeroCols . mapRows (transStepDist steps) $ transFrom
  where steps = startsSteps transTo

transStepDist :: [Dist] -> Dist -> Dist
transStepDist steps dist = sum $ (\(ix, p) -> (p *) <$> getStep ix) <$> M.vecToAssocList dist
  where getStep 0 = let len = (M.dim (head steps)) in onehotVector len len
        getStep n = steps !! (n - 1)

startsSteps :: Trans -> [Dist]
startsSteps = map (`M.row` 1) . transSteps

transSteps :: Trans -> [Trans]
transSteps m = iterate (`transStep` m) $ m

transStep :: Trans -> Trans -> Trans
transStep m1 m2 = removeEndTransitions (m1' `M.mul` m2', max r1 r2)
  where maxEnds = max (nEnds m1) (nEnds m2)
        (m1', r1) = addEndTransitions maxEnds m1
        (m2', r2) = addEndTransitions maxEnds m2

addEndTransitions :: Int -> Trans -> (Trans, Int)
addEndTransitions minEnds m = (M.vconcat [addStartColumn $ m, endTransitions], r)
  where (r, c) = M.dims m
        (_, endTransitions) = splitRowsAt r $ forwardDiagonal (r + max (nEnds m) minEnds)

removeEndTransitions :: (Trans, Int) -> Trans
removeEndTransitions (m, r) = trimZeroCols . M.delCol 1 . fst $ splitRowsAt r m
