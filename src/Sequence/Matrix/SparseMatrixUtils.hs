module Sequence.Matrix.SparseMatrixUtils where

--import qualified Math.LinearAlgebra.Sparse as M
import qualified SparseMatrix as M
import Data.Set (Set)
import qualified Data.Set as Set
import Sequence.Matrix.Types
import Data.Foldable
import Data.List (partition)

{-
inverse :: M.SparseMatrix Double -> M.SparseMatrix Double
inverse = fromDense . DM.inverse . toDense

fromDense :: DM.MatrixXd -> M.SparseMatrix Double
fromDense = M.fromRows . M.vecFromAssocList . zip [1..] . map (M.vecFromAssocList . zip [1..]) . DM.toList

toDense :: M.SparseMatrix Double -> DM.MatrixXd
toDense = DM.fromList . map allElems . allRows
-}

buildMatrix :: (Num a, Eq a)
            => (M.Index, M.Index)
            -> ((M.Index, M.Index) -> a)
            -> M.SparseMatrix a
buildMatrix (nr, nc) f = M.sparseMx [[ f (r, c)
                                     | c <- [1..nc]]
                                    | r <- [1..nr]]

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

filterCols :: (Num a) => Set M.Index -> M.SparseMatrix a -> M.SparseMatrix a
filterCols keep m = foldl (flip M.delCol) m . filter (not . (`Set.member` keep)) . reverse $ [1..M.width m]

filterRows :: (Num a) => Set M.Index -> M.SparseMatrix a -> M.SparseMatrix a
filterRows keep m = foldl (flip M.delRow) m . filter (not . (`Set.member` keep)) . reverse $ [1..M.height m]

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
normalize = M.mapOnRows normalizeVec

normalizeVec :: (Num a, Fractional a) => M.SparseVector a -> M.SparseVector a
normalizeVec r = (/ sum r) <$> r

mapRow :: (Num a, Fractional a) => Int -> (M.SparseVector a -> M.SparseVector a) -> M.SparseMatrix a -> M.SparseMatrix a
mapRow ix f m = M.replaceRow (f (M.row m ix)) ix m

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
splitColsAt ix m = ( if ix == 0
                     then M.emptyMx
                     else foldl' (\m c -> M.delCol c m) m (reverse rightCols)
                          -- M.setSize (M.height m, min ix (M.width m)) m
                   , fromCols (map (M.col m) rightCols)
                   )
  where rightCols = [ix + 1 .. M.width m]

fromCols :: (Eq a, Num a, Foldable f) => f (M.SparseVector a) -> M.SparseMatrix a
fromCols = M.trans . M.fromRows

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

toVec :: (Eq a, Num a, Foldable f) => f a -> M.SparseVector a
toVec = M.vecFromAssocList . (\l -> if null l then [(0,0)] else l) . zip [1..] . toList

scale :: (Eq a, Num a) => a -> M.SparseMatrix a -> M.SparseMatrix a
scale a = ((a *) <$>)

mapRows :: (Eq a, Num a) => (M.SparseVector a -> M.SparseVector a) -> M.SparseMatrix a -> M.SparseMatrix a
mapRows f = M.fromRows . toVec . map f . allRows
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
