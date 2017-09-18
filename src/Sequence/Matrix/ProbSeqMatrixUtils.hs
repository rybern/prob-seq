module Sequence.Matrix.ProbSeqMatrixUtils where

import qualified Math.LinearAlgebra.Sparse as M
import Sequence.Matrix.SparseMatrixUtils
import Sequence.Matrix.Types
import Sequence.Matrix.SparseMatrixUtils
import qualified Data.Vector as V
import Data.Vector (Vector)

getNormalTrans :: MatSeq s -> Trans
getNormalTrans = addFixedEndRow . addStartColumn . collapseEnds . trans

getNormalTransWithEnds :: MatSeq s -> Trans
getNormalTransWithEnds = addFixedEndRow . addStartColumn . trans

addStartColumn :: Trans -> Trans
addStartColumn trans = prependCol (M.zeroVec (M.height trans)) trans

collapseEnds :: Trans -> Trans
collapseEnds trans = M.hconcat [main, flatEnds]
  where (main, ends) = splitEnds trans
        flatEnds = setWidth 1 $ M.mapOnRows (M.singVec . sum) ends

splitStart :: Trans -> (Dist, Trans)
splitStart = M.popRow 1

splitEnds :: Trans -> (Trans, Trans)
splitEnds trans = splitColsAt (nStates trans) trans

splitTransTokens :: Trans -> (Dist, Trans, Dist, Trans)
splitTransTokens trans = (mainStart, mainTrans, endsStart, endsTrans)
  where (main, ends) = splitColsAt (nStates trans) trans
        (mainStart, mainTrans) = M.popRow 1 main
        (endsStart, endsTrans) = M.popRow 1 ends

joinTransTokens :: (Dist, Trans, Dist, Trans) -> Trans
joinTransTokens (mainStart, mainTrans, endsStart, endsTrans) =
  M.hconcat [ (prependRow mainStart mainTrans)
            , (prependRow endsStart endsTrans)
            ]

nEnds :: Trans -> Int
nEnds m = (M.width m) - (nStates m)

addFixedEndRow :: Trans -> Trans
addFixedEndRow trans = appendRow (onehotVector (M.width trans) (M.width trans)) trans

nStates :: Trans -> Int
nStates = pred . M.height

forwardDiagonal :: (Eq a, Num a) => Int -> M.SparseMatrix a
forwardDiagonal n = M.delRow 1 . M.idMx . succ $ n

mapStates :: (a -> b) -> MatSeq a -> MatSeq b
mapStates f seq = seq {stateLabels = V.map (\(a, ts) -> (f a, ts)) (stateLabels seq)}


removeLabelSeq :: MatSeq a -> MatSeq a
removeLabelSeq seq = seq { stateLabels = removeLabel (stateLabels seq) }

removeLabel :: Vector (a, StateTag) -> Vector (a, StateTag)
removeLabel = V.map (\(s, StateTag _ (t:_)) -> (s, t))

appendLabelSeq :: Int -> MatSeq a -> MatSeq a
appendLabelSeq label seq = seq { stateLabels = appendLabel label (stateLabels seq) }

appendLabel :: Int -> Vector (a, StateTag) -> Vector (a, StateTag)
appendLabel label = V.map (\(s, ts) -> (s, StateTag label [ts]))

reachableSkips :: Trans -> [Int]
reachableSkips m = map fst . filter snd . zip [0..] . map M.isNotZeroVec . drop (nStates m) . allCols $ m
