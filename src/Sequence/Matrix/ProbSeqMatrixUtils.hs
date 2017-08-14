module Sequence.Matrix.ProbSeqMatrixUtils where

import qualified Math.LinearAlgebra.Sparse as M
import Sequence.Matrix.SparseMatrixUtils
import Sequence.Matrix.Types

collapseEnds :: Trans -> Trans
collapseEnds trans = M.hconcat [main, flatEnds]
  where (main, ends) = splitEnds trans
        flatEnds = setWidth 1 $ M.mapOnRows (M.singVec . sum) ends

splitEnds :: Trans -> (Trans, Trans)
splitEnds trans = splitColsAt (nStates trans) trans

addFixedEndRow :: Trans -> Trans
addFixedEndRow trans = appendRow (onehotVector (M.width trans) (M.width trans)) trans

nStates :: Trans -> Int
nStates = pred . M.height

forwardDiagonal :: (Eq a, Num a) => Int -> M.SparseMatrix a
forwardDiagonal n = M.delRow 1 . M.idMx . succ $ n
