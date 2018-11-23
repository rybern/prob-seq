{-# LANGUAGE OverloadedLists, ViewPatterns, GeneralizedNewtypeDeriving #-}
module SparseMatrix where

--import qualified Math.LinearAlgebra.Sparse as M
import qualified Data.Eigen.SparseMatrix as M
import qualified Data.Eigen.Internal as I
import Foreign.C.Types
import qualified Data.Vector.Storable as SV
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import Data.Foldable (toList)

-- index from 1
-- fromAssocList uses max indices

type Index = Int

newtype SparseMatrix = SM M.SparseMatrixXd
  deriving Show
-- row vector, shaped (1, n)
newtype SparseVector = SV SparseMatrix
  deriving Show
type CTriplet = I.CTriplet CDouble

instance Semigroup SparseVector where
  (SV a) <> (SV b) = SV $ hconcat [a, b]

instance Monoid SparseVector where
  mempty = SV emptyMx

unSM :: SparseMatrix -> M.SparseMatrixXd
unSM (SM m) = m

unSV :: SparseVector -> SparseMatrix
unSV (SV m) = m

width :: SparseMatrix -> Int
width = M.cols . unSM

height :: SparseMatrix -> Int
height = M.rows . unSM

trans :: SparseMatrix -> SparseMatrix
trans = SM . M.transpose . unSM

toAssocList :: SparseMatrix -> [((Index, Index), Double)]
toAssocList m = (((height m, width m), 0) :)
              . map (\(x, y, v) -> ((x + 1, y + 1), v))
              . M.toList
              . unSM
              $ pruned m

toAssocList' :: SparseMatrix -> [((Index, Index), Double)]
toAssocList' = map (\(x, y, v) -> ((x, y), v)) . M.toList . unSM

fromAssocList :: [((Index, Index), Double)] -> SparseMatrix
fromAssocList ps = fromAssocList' maxR maxC ps'
  where (maxR, maxC, ps') = foldl' takeTriple (0, 0, []) ps
        takeTriple (maxR, maxC, sofar) ((r, c), v) =
          (max maxR r, max maxC c, ((r - 1, c - 1), v):sofar)

fromAssocList' :: Int -> Int -> [((Index, Index), Double)] -> SparseMatrix
fromAssocList' width height = SM . M.fromList width height . map (\((x, y), v) -> (x, y, v))

vecFromAssocList' :: Int -> [(Index, Double)] -> SparseVector
vecFromAssocList' width = SV . fromAssocList' 1 width . map (\(x, v) -> ((0, x), v))

vecFromAssocList :: [(Index, Double)] -> SparseVector
vecFromAssocList ps = if maxC == 0
                      then emptyVec
                      else SV $ fromAssocList' 1 maxC ps'
  where (maxC, ps') = foldl' takeTriple (0, []) ps
        takeTriple (maxC, sofar) (c, v) =
          (max maxC c, ((0, c - 1), v):sofar)

vecToAssocList :: SparseVector -> [(Index, Double)]
vecToAssocList = map (\((_, x), v) -> (x, v)) . toAssocList . unSV

sparseMx :: [[Double]] -> SparseMatrix
sparseMx = SM . M.fromDenseList

addMx :: SparseMatrix -> SparseMatrix -> SparseMatrix
addMx a b = SM $ (unSM $ update a) + (unSM $ update b)
  where (ah, aw) = dims a
        (bh, bw) = dims b
        dims' = (max ah bh, max aw bw)
        update m = if dims m /= dims'
                   then setSize dims' m
                   else m

instance Num SparseMatrix where
  (+) = addMx
  (*) (SM a) (SM b) = SM $ a * b
  negate (SM a) = SM $ negate a
  abs (SM a) = SM $ abs a
  signum (SM a) = SM $ signum a
  fromInteger i = SM $ fromInteger i

instance Num SparseVector where
  (+) = addVec
  (*) (SV a) (SV b) = SV $ a * b
  negate (SV a) = SV $ negate a
  abs (SV a) = SV $ abs a
  signum (SV a) = SV $ signum a
  fromInteger i = SV $ fromInteger i

addVec :: SparseVector -> SparseVector -> SparseVector
addVec a b = SV $ addMx (unSV a) (unSV b)

mul :: SparseMatrix -> SparseMatrix -> SparseMatrix
mul a b = if aw == bh
          then SM $ a `mul'` b
          else if bh < aw
               then SM $ a `mul'` setSize (aw, bw) b
               else SM $ a `mul'` b -- throws an error
  where (ah, aw) = dims a
        (bh, bw) = dims b
        mul' (SM a) (SM b) = a `M.mul` b

sumMx :: SparseMatrix -> Double
sumMx = realToFrac . SV.sum . M.values . unSM

sumV :: SparseVector -> Double
sumV = realToFrac . SV.sum . M.values . unSM . unSV

scale ::  Double -> SparseMatrix -> SparseMatrix
scale x = SM . M.scale x . unSM

scaleV ::  Double -> SparseVector -> SparseVector
scaleV x = SV . scale x . unSV

allElems :: SparseVector -> Vector Double
allElems v = V.map (v !) $ [1..dim v]

(!) :: SparseVector -> Index -> Double
(!) (SV (SM v)) ix = v M.! (0, ix - 1)

lookupDefault0 :: SparseMatrix -> (Index, Index) -> Double
lookupDefault0 m (r, c) =
  let (h, w) = dims m
  in if r <= h && c <= w
     then m # (r, c)
     else 0


(#) :: SparseMatrix -> (Index, Index) -> Double
(#) (SM m) (r, c) = m M.! (r - 1, c - 1)

dims :: SparseMatrix -> (Int, Int)
dims m = (height m, width m)

dim :: SparseVector -> Int
dim = width . unSV

isZeroMx :: SparseMatrix -> Bool
isZeroMx = (== 0) . M.nonZeros . unSM

isEmptyMx :: SparseMatrix -> Bool
isEmptyMx = (== (0,0)) . dims

isEmptyVec :: SparseVector -> Bool
isEmptyVec = isEmptyMx . unSV

isZeroVec :: SparseVector -> Bool
isZeroVec = isZeroMx . unSV

isNotZeroVec :: SparseVector -> Bool
isNotZeroVec = not . isZeroVec

zeroMx :: (Int, Int) -> SparseMatrix
zeroMx (height, width) = SM $ M.fromList height width []

reduceFlatMx :: SparseMatrix -> SparseMatrix
reduceFlatMx m = let (r, c) = dims m
                 in if r == 0 || c == 0
                    then emptyMx
                    else m

emptyMx :: SparseMatrix
emptyMx = zeroMx (0, 0)

emptyVec :: SparseVector
emptyVec = SV $ zeroMx (0, 0)

idMx :: Int -> SparseMatrix
idMx n = fromAssocList' n n [((i, i), 1) | i <- [0..n-1]]

singVec :: Double -> SparseVector
singVec x = SV $ fromAssocList' 1 1 [((0, 0), x)]

zeroVec :: Int -> SparseVector
zeroVec n = SV $ zeroMx (1, n)

row :: SparseMatrix -> Index -> SparseVector
row m ix = if isEmptyMx m
           then emptyVec
           else SV . SM $ M.block (ix - 1) 0 1 (width m) (unSM m)

col :: SparseMatrix -> Index -> SparseVector
col m ix = if isEmptyMx m
           then emptyVec
           else SV . SM $ M.transpose $ M.block 0 (ix - 1) (height m) 1 (unSM m)

-- would it be better to iterate over the list form?
rows :: SparseMatrix -> Vector SparseVector
rows m = V.map (row m) $ [1..height m]

cols :: SparseMatrix -> Vector SparseVector
cols m = V.map (col m) $ [1..width m]

fromRows :: (Foldable f, Functor f) => f SparseVector -> SparseMatrix
fromRows = vconcat . fmap unSV . toList

fromCols :: (Foldable f, Functor f) => f SparseVector -> SparseMatrix
fromCols = hconcat . fmap (trans . unSV) . toList

blockMx :: [[SparseMatrix]] -> SparseMatrix
blockMx [] = emptyMx
blockMx mxs = SM
            . M.fromVector totalHeight totalWidth
            . vconcat'
            . zip rowIxs
            . map (hconcat' . zip colIxs . map (M.toVector . unSM))
            $ mxs
  where heights = map (maximum . map height) $ mxs
        totalHeight = sum heights
        rowIxs = scanl (+) 0 heights
        widths = map width . head $ mxs
        totalWidth = sum widths
        colIxs = scanl (+) 0 widths

vconcat :: [SparseMatrix] -> SparseMatrix
vconcat [] = emptyMx
vconcat (map reduceFlatMx -> mxs) =
  SM . M.fromVector totalHeight totalWidth . vconcat' . zip colIxs . map (M.toVector . unSM) $ mxs
  where heights = map height mxs
        totalHeight = sum heights
        colIxs = scanl (+) 0 heights
        totalWidth = maximum . map width $ mxs

hconcat :: [SparseMatrix] -> SparseMatrix
hconcat [] = emptyMx
hconcat (map reduceFlatMx -> mxs) =
  SM . M.fromVector totalHeight totalWidth . hconcat' . zip rowIxs . map (M.toVector . unSM) $ mxs
  where widths = map width mxs
        totalWidth = sum widths
        rowIxs = scanl (+) 0 widths
        totalHeight = maximum . map height $ mxs

shiftTriplet :: (Int, Int) -> CTriplet -> CTriplet
shiftTriplet (a, b) (I.CTriplet x y v) = I.CTriplet (x + fromIntegral a) (y + fromIntegral b) v

shiftTriplets :: (Int, Int) -> SV.Vector CTriplet -> SV.Vector CTriplet
shiftTriplets by = SV.map (shiftTriplet by)

vconcat' :: (Foldable f) => f (Int, SV.Vector CTriplet) -> SV.Vector CTriplet
vconcat' = foldMap (\(rowIx, tris) -> shiftTriplets (rowIx, 0) tris)

hconcat' :: (Foldable f) => f (Int, SV.Vector CTriplet) -> SV.Vector CTriplet
hconcat' = foldMap (\(colIx, tris) -> shiftTriplets (0, colIx) tris)

shiftAndJoin :: (Int, Int) -> SparseMatrix -> SparseMatrix -> SparseMatrix
shiftAndJoin by@(x, y) m1 m2 = SM
                             . M.fromVector height' width'
                             . (M.toVector (unSM m2) SV.++)
                             . shiftTriplets by
                             $ M.toVector (unSM m1)
  where height' = max (height m1 + x) (height m2)
        width' = max (width m1 + y) (width m2)

overwrite' :: ((Int, Int) -> Bool)
          -> [((Index, Index), Double)]
          -> SparseMatrix
          -> SparseMatrix
overwrite' p vals m = fromAssocList' (height m) (width m)
                   . (vals ++)
                   . filter (not . p . fst)
                   . toAssocList'
                   $ m

 -- unclear which is faster
replaceRow' :: SparseVector -> Index -> SparseMatrix -> SparseMatrix
replaceRow' v (pred -> r) = overwrite'
  ((== r) . fst)
  (map (\((_, c), v) -> ((r, c), v)) $ toAssocList' (unSV v))

replaceRow :: SparseVector -> Index -> SparseMatrix -> SparseMatrix
replaceRow (SV row) (pred -> r) m = vconcat . catMaybes $
  [ if r == 0 then Nothing else Just . SM $
    M.block 0 0 r (width m) (unSM m)
  , Just row
  , if r == height m - 1 then Nothing else Just . SM $
    M.block (r+1) 0 (height m - r - 1) (width m) (unSM m)]

testMat :: Int -> Int -> Double -> SparseMatrix
testMat height width start = sparseMx [ [start+fromIntegral width*row
                                          .. start+fromIntegral width*(row+1)-1]
                                      | row <- [0..fromIntegral height-1]]

mapOnRows :: (SparseVector -> SparseVector) -> SparseMatrix -> SparseMatrix
mapOnRows f = fromRows . V.map f . rows

unionVecsWith :: (Double -> Double -> Double) -> SparseVector -> SparseVector -> SparseVector
unionVecsWith f v1 v2 = vecFromAssocList' dim'
                      . Map.toList
                      $ Map.unionWith f (toMap v1) (toMap v2)
  where toMap = Map.fromList . vecToAssocList
        dim' = max (dim v1) (dim v2)

vecIns :: SparseVector -> (Index, Double) -> SparseVector
vecIns (SV v) (pred -> c, val) = SV $ overwrite' ((== c) . snd) [((0, c), val)] v

addRow :: SparseVector -> Index -> SparseMatrix -> SparseMatrix
addRow (SV row) (pred -> r) m = vconcat . catMaybes $
  [ if r == 0 then Nothing else Just . SM $
    M.block 0 0 r (width m) (unSM m)
  , Just row
  , if r == height m then Nothing else Just . SM $
    M.block r 0 (height m - r) (width m) (unSM m)]

addCol :: SparseVector -> Index -> SparseMatrix -> SparseMatrix
addCol (SV col) (pred -> c) m = hconcat . catMaybes $
  [ if c == 0 then Nothing else Just . SM $
    M.block 0 0 (height m) c (unSM m)
  , Just $ trans col
  , if c == width m then Nothing else Just . SM $
    M.block 0 c (height m) (width m - c) (unSM m)]

setSize :: (Int, Int) -> SparseMatrix -> SparseMatrix
setSize (h, w) = SM . M.fromVector h w . M.toVector . unSM

setLength :: Int -> SparseVector -> SparseVector
setLength l = SV . setSize (1, l) . unSV

delRow :: Index -> SparseMatrix -> SparseMatrix
delRow (pred -> r) m = vconcat . catMaybes $
  [ if r == 0 then Nothing else Just . SM$
    M.block 0 0 r (width m) (unSM m)
  , if r == height m then Nothing else Just . SM$
    M.block (r+1) 0 (height m - r - 1) (width m) (unSM m)]

delCol :: Index -> SparseMatrix -> SparseMatrix
delCol (pred -> c) m = hconcat . catMaybes $
  [ if c == 0 then Nothing else Just . SM$
    M.block 0 0 (height m) c (unSM m)
  , if c == width m then Nothing else Just . SM$
    M.block 0 (c+1) (height m) (width m - c - 1) (unSM m)]

popRow :: Index -> SparseMatrix -> (SparseVector, SparseMatrix)
popRow r m = if isEmptyMx m
             then (emptyVec, emptyMx)
             else (row m r, delRow r m)

popCol :: Index -> SparseMatrix -> (SparseVector, SparseMatrix)
popCol c m = if isEmptyMx m
             then (emptyVec, emptyMx)
             else (col m c, delCol c m)

pruned :: SparseMatrix -> SparseMatrix
pruned = SM . M.pruned 0 . unSM

{-
testA = testMat 3 4 1
testB = testMat 3 3 13
testC = testMat 1 4 22
testD = testMat 1 3 26

test1 = blockMx [ [testA, testB]
                , [testC, testD]]

test2 = replaceRow (SV $ sparseMx [[-1, -2, -3, -4, -5, -6, -7]]) 2 test1
test3 = replaceRow' (SV $ sparseMx [[-1, -2, -3, -4, -5, -6, -7]]) 2 test1
test4 = addRow (SV $ sparseMx [[-1, -2, -3, -4, -5, -6, -7]]) 2 test1
test5 = addCol (SV $ sparseMx [[-1, -2, -3, -4]]) 1 test1
-}

{-
popRow :: (Num a) => Index -> SparseMatrix a -> (SparseVector a, SparseMatrix a)
popRow = M.popRow
-}
