module SMoL.Matrix.SparseMatrixUtils where

--import qualified Math.LinearAlgebra.Sparse as M
import qualified SparseMatrix as M
import Data.Set (Set)
import qualified Data.Set as Set
import SMoL.Matrix.Types
import Data.Foldable
import Data.List (partition)
import Data.Vector (Vector)
import qualified Data.Vector as V

{-
inverse :: M.SparseMatrix Double -> M.SparseMatrix Double
inverse = fromDense . DM.inverse . toDense

fromDense :: DM.MatrixXd -> M.SparseMatrix Double
fromDense = M.fromRows . M.vecFromAssocList . zip [1..] . map (M.vecFromAssocList . zip [1..]) . DM.toList

toDense :: M.SparseMatrix Double -> DM.MatrixXd
toDense = DM.fromList . map allElems . allRows
-}

buildMatrixM :: (Monad m)
             => (M.Index, M.Index)
             -> ((M.Index, M.Index) -> m Double)
             -> m M.SparseMatrix
buildMatrixM (nr, nc) f =
  M.sparseMx <$> mapM (mapM f) [[ (r, c)
                                | c <- [1..nc]]
                               | r <- [1..nr]]

buildMatrix :: (M.Index, M.Index)
            -> ((M.Index, M.Index) -> Double)
            -> M.SparseMatrix
buildMatrix (nr, nc) f = M.sparseMx [[ f (r, c)
                                     | c <- [1..nc]]
                                    | r <- [1..nr]]

mapWithIxs :: ((M.Index, M.Index) -> Double -> Double) -> M.SparseMatrix -> M.SparseMatrix
mapWithIxs fn = M.fromAssocList . map (\(ixs, a) -> (ixs, fn ixs a)) . M.toAssocList

reverseVec :: M.SparseVector -> M.SparseVector
reverseVec m = M.setLength (M.dim m)
               . M.vecFromAssocList
               . zip [1..]
               . reverse
               . V.toList
               . M.allElems
               $ m

reverseCols :: M.SparseMatrix -> M.SparseMatrix
reverseCols m = M.setSize (M.dims m) . M.trans . M.fromRows . V.reverse . M.rows . M.trans $ m

reverseRows :: M.SparseMatrix -> M.SparseMatrix
reverseRows = M.fromRows . V.reverse . M.rows

prependRow :: M.SparseVector -> M.SparseMatrix -> M.SparseMatrix
prependRow v = M.addRow v 1

prependCol :: M.SparseVector -> M.SparseMatrix -> M.SparseMatrix
prependCol v = M.addCol v 1

appendRow :: M.SparseVector -> M.SparseMatrix -> M.SparseMatrix
appendRow v m = M.addRow v (succ (M.height m)) m

appendCol :: M.SparseVector -> M.SparseMatrix -> M.SparseMatrix
appendCol v m = M.addCol v (succ (M.width m)) m

popCol :: M.Index -> M.SparseMatrix -> (M.SparseVector, M.SparseMatrix)
popCol i m = (M.col m i, M.delCol i m)

filterCols :: Set M.Index -> M.SparseMatrix -> M.SparseMatrix
filterCols keep m = foldl (flip M.delCol) m . filter (not . (`Set.member` keep)) . reverse $ [1..M.width m]

filterRows :: Set M.Index -> M.SparseMatrix -> M.SparseMatrix
filterRows keep m = foldl (flip M.delRow) m . filter (not . (`Set.member` keep)) . reverse $ [1..M.height m]

trimZeroCols :: M.SparseMatrix -> M.SparseMatrix
trimZeroCols m = let (lastCol, m') = popLastCol m
                 in if M.isZeroVec lastCol
                    then trimZeroCols m'
                    else m

popLastCol :: M.SparseMatrix -> (M.SparseVector, M.SparseMatrix)
popLastCol m = (M.col m lastIx, M.delCol lastIx m)
  where lastIx = M.width m

onehotVector :: M.Index -> Int -> M.SparseVector
onehotVector hot len = M.vecIns (M.zeroVec len) (hot, 1.0)

normalizeMat :: M.SparseMatrix -> M.SparseMatrix
normalizeMat = M.mapOnRows normalizeVec

normalizeVec :: M.SparseVector -> M.SparseVector
normalizeVec r = (recip $ M.sumV r) `M.scaleV` r

mapRow :: Int -> (M.SparseVector -> M.SparseVector) -> M.SparseMatrix -> M.SparseMatrix
mapRow ix f m = M.replaceRow (f (M.row m ix)) ix m

colMx :: M.SparseVector -> M.SparseMatrix
colMx = M.trans . M.fromRows . (\x -> [x])

diagConcat :: M.SparseMatrix -> M.SparseMatrix -> M.SparseMatrix
diagConcat a d = M.blockMx [ [a, blockB]
                           , [blockC, d] ]
  where (rA, cA) = M.dims a
        (rD, cD) = M.dims d
        blockB = M.zeroMx (rA, cD)
        blockC = M.zeroMx (rD, cA)

splitVecAt :: M.Index -> M.SparseVector -> (M.SparseVector, M.SparseVector)
splitVecAt pivot = (\(a, b) -> ( M.vecFromAssocList a
                               , M.vecFromAssocList (map (\(ix, val) -> (ix - pivot, val)) b)))
                   . break ((> pivot) . fst)
                   . tail
                   . M.vecToAssocList


splitRowsAt :: M.Index -> M.SparseMatrix -> (M.SparseMatrix, M.SparseMatrix)
splitRowsAt ix = (\(a, b) -> (M.fromRows a, M.fromRows b)) . V.splitAt ix . allRows

splitColsAt :: M.Index -> M.SparseMatrix -> (M.SparseMatrix, M.SparseMatrix)
splitColsAt ix = (\(a, b) -> (M.fromCols a, M.fromCols b)) . V.splitAt ix . allCols

splitColsAt' :: M.Index -> M.SparseMatrix -> (M.SparseMatrix, M.SparseMatrix)
splitColsAt' ix m = ( if ix == 0
                     then M.emptyMx
                     else foldl' (\m c -> M.delCol c m) m (reverse rightCols)
                          -- M.setSize (M.height m, min ix (M.width m)) m
                   , M.fromCols (map (M.col m) rightCols)
                   )
  where rightCols = [ix + 1 .. M.width m]

setWidth :: Int -> M.SparseMatrix -> M.SparseMatrix
setWidth w m = M.setSize (M.height m, w) m

setHeight :: Int -> M.SparseMatrix -> M.SparseMatrix
setHeight h m = M.setSize (h, M.width m) m

allCols :: M.SparseMatrix -> Vector M.SparseVector
allCols = M.cols

allRows :: M.SparseMatrix -> Vector M.SparseVector
allRows = M.rows

toVec :: (Foldable f) => f Double -> M.SparseVector
toVec = M.vecFromAssocList . (\l -> if null l then [(0,0)] else l) . zip [1..] . toList

mapRows :: (M.SparseVector -> M.SparseVector) -> M.SparseMatrix -> M.SparseMatrix
mapRows f = M.fromRows . V.map f . allRows
  --where rows = f <$> allRows m
        --maxLen = maximum (M.dim <$> rows)
{-
splitColsAt :: (Eq a, Fractional a) => M.Index -> M.SparseMatrix -> (M.SparseMatrix, M.SparseMatrix)
splitColsAt ix m = (\(left, rightList) -> (left, fromCols rightList)) . (!! nPull) . iterate pullCol $ (m, [])
  where pullCol (left, rightList) = let (col, left') = popLastCol left
                                    in (left', col:rightList)
        nPull = M.width m - ix
-}

-- useful later?
-- maybeBlocksMx :: (Eq a, Fractional a) => [[M.SparseMatrix]] -> M.SparseMatrix
