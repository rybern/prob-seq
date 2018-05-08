module SparseMatrix where

import qualified Math.LinearAlgebra.Sparse as M

type Index = M.Index

type SparseMatrix a = M.SparseMatrix a
type SparseVector a = M.SparseVector a

sparseMx :: (Num a, Eq a) => [[a]] -> SparseMatrix a
sparseMx = M.sparseMx

fromAssocList :: (Num a, Eq a) => [((Index, Index), a)] -> SparseMatrix a
fromAssocList = M.fromAssocList

vecFromAssocList :: (Num a, Eq a) => [(Index, a)] -> SparseVector a
vecFromAssocList = M.vecFromAssocList

toAssocList :: (Num a, Eq a) => SparseMatrix a -> [((Index, Index), a)]
toAssocList = M.toAssocList

vecToAssocList :: (Num a, Eq a) => SparseVector a -> [(Index, a)]
vecToAssocList = M.vecToAssocList

setSize :: (Num a) => (Int, Int) -> SparseMatrix a -> SparseMatrix a
setSize = M.setSize

setLength :: (Num a, Eq a) => Int -> SparseVector a -> SparseVector a
setLength = M.setLength

rows :: (Num a, Eq a) => SparseMatrix a -> SparseVector (SparseVector a)
rows = M.rows

fromRows :: (Num a, Eq a, Foldable f) => f (SparseVector a) -> SparseMatrix a
fromRows = M.fromRows

dims :: (Num a, Eq a) => SparseMatrix a -> (Int, Int)
dims = M.dims

dim :: (Num a) => SparseVector a -> Int
dim = M.dim

width :: (Num a) => SparseMatrix a -> Int
width = M.width

height :: (Num a) => SparseMatrix a -> Int
height = M.height

trans :: (Num a, Eq a) => SparseMatrix a -> SparseMatrix a
trans = M.trans

addRow :: (Num a) => SparseVector a -> Int -> SparseMatrix a -> SparseMatrix a
addRow = M.addRow

addCol :: (Num a) => SparseVector a -> Int -> SparseMatrix a -> SparseMatrix a
addCol = M.addRow

row :: (Num a) => SparseMatrix a -> Index -> SparseVector a
row = M.row

delRow :: (Num a) => Index -> SparseMatrix a -> SparseMatrix a
delRow = M.delRow

popRow :: (Num a) => Index -> SparseMatrix a -> (SparseVector a, SparseMatrix a)
popRow = M.popRow

col :: (Num a, Eq a) => SparseMatrix a -> Index -> SparseVector a
col = M.col

delCol :: (Num a) => Index -> SparseMatrix a -> SparseMatrix a
delCol = M.delCol

isZeroMx :: (Num a, Eq a) => SparseMatrix a -> Bool
isZeroMx = M.isZeroMx

isZeroVec :: (Num a, Eq a) => SparseVector a -> Bool
isZeroVec = M.isZeroVec

isNotZeroVec :: (Num a, Eq a) => SparseVector a -> Bool
isNotZeroVec = M.isNotZeroVec

vecIns :: (Num a, Eq a) => SparseVector a -> (Index, a) -> SparseVector a
vecIns = M.vecIns

replaceRow :: (Num a) => SparseVector a -> Index -> SparseMatrix a -> SparseMatrix a
replaceRow = M.replaceRow

blockMx :: (Num a, Eq a) => [[SparseMatrix a]]-> SparseMatrix a
blockMx = M.blockMx

emptyMx :: (Num a, Eq a) => SparseMatrix a
emptyMx = M.emptyMx

zeroMx :: (Num a, Eq a) => (Int, Int) -> SparseMatrix a
zeroMx = M.zeroMx

idMx :: (Num a, Eq a) => Int -> SparseMatrix a
idMx = M.idMx

singVec :: (Num a, Eq a) => a -> SparseVector a
singVec = M.singVec

zeroVec :: (Num a, Eq a) => Int -> SparseVector a
zeroVec = M.zeroVec

(!) :: (Num a) => SparseVector a -> Index -> a
(!) = (M.!)

(#) :: (Num a) => SparseMatrix a -> (Index, Index) -> a
(#) = (M.#)

mapOnRows :: (SparseVector a -> SparseVector b) -> SparseMatrix a -> SparseMatrix b
mapOnRows = M.mapOnRows

hconcat :: (Num a, Eq a) => [SparseMatrix a]-> SparseMatrix a
hconcat = M.hconcat

vconcat :: (Num a, Eq a) => [SparseMatrix a]-> SparseMatrix a
vconcat = M.vconcat

unionVecsWith :: (a -> a -> a) -> SparseVector a -> SparseVector a -> SparseVector a
unionVecsWith = M.unionVecsWith

mul :: (Num a, Eq a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
mul = M.mul
