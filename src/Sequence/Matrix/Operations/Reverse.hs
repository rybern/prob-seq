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

dropoutDist :: [Prob] -> [Prob]
dropoutDist [] = []
dropoutDist (p:rest) = p : dropoutDist (map (/ (1 - p)) rest)

-- NOT SURE IF backward WORKS CORRECTLY. really should take more time to test it.
priors m = flip M.vecIns (1, 1.0)
            . flip M.vecIns (M.width m, 1.0)
            . ((/ fromIntegral nTimes) <$>)
            . Prelude.sum
            . Prelude.take (M.height m * nTimes)
            . (\m' -> iterate (`M.mulVM` m') (onehotVector 1 (M.height m)))
            . M.replaceRow (onehotVector 1 (M.width m)) (M.height m)
            $ m
  where nTimes = 100

reverseT ::
  (Eq a, Fractional a) => M.SparseMatrix a -> M.SparseMatrix a
reverseT m = normalize
             . flip M.ins ((1,1), 0)
             . flip M.ins ((M.height m, M.width m), 1.0)
             . mapWithIxs bayes
             $ M.trans flipped
  where ps' = priors m
        ps = reverseVec ps'

        flipped = reverseRows . reverseCols $ m
        --flipped = reverseRows . reverseCols . snd . M.popRow (M.height m) $ m
        bayes (r, c) val = (ps M.! r) * val / (ps M.! c)

reverseSequence' :: MatSeq s -> MatSeq s
reverseSequence' s = MatSeq {
    trans = let m = getNormalTrans s
            in snd . popCol 1 . snd . M.popRow (M.height m) $ reverseT m
  , stateLabels = V.reverse (stateLabels s)
  }
