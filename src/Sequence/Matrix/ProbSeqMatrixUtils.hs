module Sequence.Matrix.ProbSeqMatrixUtils where

--import qualified Math.LinearAlgebra.Sparse as M
import qualified SparseMatrix as M
import Sequence.Matrix.SparseMatrixUtils
import Sequence.Matrix.Types
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Vector (Vector)
import qualified Data.Vector as V

cleanTrans :: Trans -> Trans
cleanTrans = addStartColumn . collapseEnds

unCleanTrans :: Trans -> Trans
unCleanTrans t = (snd . M.popRow (M.height t) $ t)

getNormalTrans :: MatSeq s -> Trans
getNormalTrans = addFixedEndRow . addStartColumn . collapseEnds . trans

getNormalTransWithEnds :: MatSeq s -> Trans
getNormalTransWithEnds = addFixedEndRow . addStartColumn . trans

addStartColumn :: Trans -> Trans
addStartColumn trans = prependCol (M.zeroVec (M.height trans)) trans

collapseEnds :: Trans -> Trans
collapseEnds trans = M.hconcat [main, flatEnds]
  where (main, ends) = splitEnds trans
        flatEnds = setWidth 1 $ M.mapOnRows (M.singVec . M.sumV) ends

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

forwardDiagonal :: Int -> M.SparseMatrix
forwardDiagonal n = M.delRow 1 . M.idMx . succ $ n

mapStates :: (a -> b) -> MatSeq a -> MatSeq b
mapStates f seq = seq {stateLabels = V.map (\(StateLabel a t s) ->
                                              (StateLabel (f a) t s)) (stateLabels seq)}


removeLabelSeq :: MatSeq a -> MatSeq a
removeLabelSeq seq = seq { stateLabels = removeLabel (stateLabels seq) }

removeLabel :: Vector (StateLabel s) -> Vector (StateLabel s)
removeLabel = V.map (\(StateLabel l (StateTag _ (t:_)) s) -> StateLabel l t s)

appendLabelSeq :: Int -> MatSeq a -> MatSeq a
appendLabelSeq label seq = seq { stateLabels = appendLabel label (stateLabels seq) }

appendLabel :: Int -> Vector (StateLabel s) -> Vector (StateLabel s)
appendLabel label = V.map (\(StateLabel l t s) ->
                              (StateLabel l (StateTag label [t]) s))

appendLabel' :: Maybe Int -> Int -> Vector (StateLabel s) -> Vector (StateLabel s)
appendLabel' mTagID label = V.map (\(StateLabel l t s) ->
                                    (StateLabel
                                      l
                                      (StateTag label [t])
                                      (maybe s (\tagID -> IntMap.insert tagID label s) mTagID)))

reachableSkips :: Trans -> [Int]
reachableSkips m = map fst . filter snd . zip [0..] . map M.isNotZeroVec . drop (nStates m) . V.toList . allCols $ m

stationary :: Int -> Trans
stationary nStates = appendCol (M.zeroVec (nStates + 1)) . prependRow (M.zeroVec nStates) $ M.idMx nStates
