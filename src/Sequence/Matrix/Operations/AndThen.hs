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

andThen :: MatSeq s -> MatSeq s -> MatSeq s
andThen seqA seqB = MatSeq {
    trans = trans'
  , stateLabels = stateLabels'
  }
  where stateLabels' = stateLabels seqA <> stateLabels seqB

        transA = trans seqA
        transB = trans seqB
        (mainA, endsA) = splitEnds transA
        (_, nonstartB) = splitStart transB
        transition = transB `distributeEnds` endsA
        rightLen = max (M.width transition) (M.width transB)
        lowerLeft = M.zeroMx (M.height nonstartB, M.width mainA)
        trans' = M.blockMx [ [mainA, setWidth rightLen transition]
                           , [lowerLeft, setWidth rightLen nonstartB] ]

instance Monoid (MatSeq a) where
  mappend a b = a `andThen` b
  mempty = emptySequence

{- STATE STEPPING -}

distributeEnds :: Trans -> Trans -> Trans
distributeEnds trans = trimZeroCols . mapRows (distributeEndDist trans)

distributeEndDist :: Trans -> Dist -> Dist
distributeEndDist trans = (`M.row` 1) . transStepDist trans

transStepDist :: Trans -> Dist -> Trans
transStepDist m dist = sum $ (\(ix, p) -> (p *) <$> transNSteps m ix) <$> M.vecToAssocList dist

transNSteps :: Trans -> Int -> Trans
transNSteps m 0 = M.idMx (M.width m)
transNSteps m n = (!! (n - 1)) . iterate (`transStep` m) $ m

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