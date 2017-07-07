module Sequence.Matrix.Utils where

import qualified Math.LinearAlgebra.Sparse as M
import Sequence.Matrix.Types

{-
inverse :: M.SparseMatrix Double -> M.SparseMatrix Double
inverse = fromDense . DM.inverse . toDense

fromDense :: DM.MatrixXd -> M.SparseMatrix Double
fromDense = M.fromRows . M.vecFromAssocList . zip [1..] . map (M.vecFromAssocList . zip [1..]) . DM.toList

toDense :: M.SparseMatrix Double -> DM.MatrixXd
toDense = DM.fromList . map allElems . allRows
-}

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

mapWithIxs :: (Num a, Eq a) => ((M.Index, M.Index) -> a -> a) -> M.SparseMatrix a -> M.SparseMatrix a
mapWithIxs fn = M.fromAssocList . map (\(ixs, a) -> (ixs, fn ixs a)) . M.toAssocList

reverseVec :: (Eq a, Num a) => M.SparseVector a -> M.SparseVector a
reverseVec m = M.setLength (M.dim m)
               . M.vecFromAssocList
               . zip [1..]
               . reverse
               . allElems
               $ m

reverseCols :: (Eq a, Num a) => M.SparseMatrix a -> M.SparseMatrix a
reverseCols m = M.setSize (M.dims m) . M.trans . M.fromRows . reverseVec . M.rows . M.trans $ m

reverseRows :: (Eq a, Num a) => M.SparseMatrix a -> M.SparseMatrix a
reverseRows = M.fromRows . reverseVec . M.rows

prependRow :: (Num a) => M.SparseVector a -> M.SparseMatrix a -> M.SparseMatrix a
prependRow v = M.addRow v 1

prependCol :: (Num a) => M.SparseVector a -> M.SparseMatrix a -> M.SparseMatrix a
prependCol v = M.addCol v 1

appendRow :: (Num a) => M.SparseVector a -> M.SparseMatrix a -> M.SparseMatrix a
appendRow v m = M.addRow v (succ (M.height m)) m

appendCol :: (Num a) => M.SparseVector a -> M.SparseMatrix a -> M.SparseMatrix a
appendCol v m = M.addCol v (succ (M.width m)) m

popCol :: (Eq a, Num a) => M.Index -> M.SparseMatrix a -> (M.SparseVector a, M.SparseMatrix a)
popCol i m = (M.col m i, M.delCol i m)

trimZeroCols :: (Eq a, Num a) => M.SparseMatrix a -> M.SparseMatrix a
trimZeroCols m = let (lastCol, m') = popLastCol m
                 in if M.isZeroVec lastCol
                    then trimZeroCols m'
                    else m

popLastCol :: (Eq a, Num a) => M.SparseMatrix a -> (M.SparseVector a, M.SparseMatrix a)
popLastCol m = (M.col m lastIx, M.delCol lastIx m)
  where lastIx = M.width m

onehotVector :: (Eq a, Fractional a) => M.Index -> Int -> M.SparseVector a
onehotVector hot len = M.vecIns (M.zeroVec len) (hot, 1.0)

normalize :: (Num a, Fractional a) => M.SparseMatrix a -> M.SparseMatrix a
normalize = M.mapOnRows (\r -> (/ sum r) <$> r)

colMx :: (Eq a, Num a, Fractional a) => M.SparseVector a -> M.SparseMatrix a
colMx = M.trans . M.fromRows . (\x -> [x])

diagConcat :: (Eq a, Fractional a) => M.SparseMatrix a -> M.SparseMatrix a -> M.SparseMatrix a
diagConcat a d = M.blockMx [ [a, blockB]
                           , [blockC, d] ]
  where (rA, cA) = M.dims a
        (rD, cD) = M.dims d
        blockB = M.zeroMx (rA, cD)
        blockC = M.zeroMx (rD, cA)

splitVecAt :: (Eq a, Num a) => M.Index -> M.SparseVector a -> (M.SparseVector a, M.SparseVector a)
splitVecAt pivot = (\(a, b) -> ( M.vecFromAssocList a
                               , M.vecFromAssocList (map (\(ix, val) -> (ix - pivot, val)) b)))
                   . break ((> pivot) . fst)
                   . tail
                   . M.vecToAssocList


splitRowsAt :: (Eq a, Fractional a) => M.Index -> M.SparseMatrix a -> (M.SparseMatrix a, M.SparseMatrix a)
splitRowsAt ix = (\(a, b) -> (M.fromRows a, M.fromRows b)) . splitAt ix . allRows

splitColsAt :: (Eq a, Fractional a) => M.Index -> M.SparseMatrix a -> (M.SparseMatrix a, M.SparseMatrix a)
splitColsAt ix = (\(a, b) -> (M.trans a, M.trans b)) . splitRowsAt ix . M.trans

fromCols :: (Eq a, Num a, Foldable f) => f (M.SparseVector a) -> M.SparseMatrix a
fromCols = M.trans . M.fromRows

forwardDiagonal :: (Eq a, Num a) => Int -> M.SparseMatrix a
forwardDiagonal n = M.delRow 1 . M.idMx . succ $ n

setWidth :: (Num a) => Int -> M.SparseMatrix a -> M.SparseMatrix a
setWidth w m = M.setSize (M.height m, w) m

setHeight :: (Num a) => Int -> M.SparseMatrix a -> M.SparseMatrix a
setHeight h m = M.setSize (h, M.width m) m

allElems :: (Num a) => M.SparseVector a -> [a]
allElems v = map (v M.!) $ [1..M.dim v]

allCols :: (Eq a, Num a) => M.SparseMatrix a -> [M.SparseVector a]
allCols = allRows . M.trans

allRows :: (Eq a, Num a) => M.SparseMatrix a -> [M.SparseVector a]
allRows = allElems . M.rows

listToVec :: (Eq a, Num a) => [a] -> M.SparseVector a
listToVec = M.vecFromAssocList . zip [1..]

mapRows :: (Eq a, Num a) => (M.SparseVector a -> M.SparseVector a) -> M.SparseMatrix a -> M.SparseMatrix a
mapRows f = M.fromRows . listToVec . map f . allRows
  --where rows = f <$> allRows m
        --maxLen = maximum (M.dim <$> rows)
{-
splitColsAt :: (Eq a, Fractional a) => M.Index -> M.SparseMatrix a -> (M.SparseMatrix a, M.SparseMatrix a)
splitColsAt ix m = (\(left, rightList) -> (left, fromCols rightList)) . (!! nPull) . iterate pullCol $ (m, [])
  where pullCol (left, rightList) = let (col, left') = popLastCol left
                                    in (left', col:rightList)
        nPull = M.width m - ix
-}

-- useful later?
-- maybeBlocksMx :: (Eq a, Fractional a) => [[M.SparseMatrix a]] -> M.SparseMatrix a
