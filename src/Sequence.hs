module Sequence where

import qualified Math.LinearAlgebra.Sparse as M
import qualified Data.Vector as V
import Data.List (find)
import Data.Monoid
import MatrixUtils
import Types

-- Do we include the first column? There should never be a ->start reference, but not having the column there might get confusing, since it needs to be added back to be used as a transition matrix.

-- do need a way to differentiate matrices with/without the added end transitions

-- trans in sequence won't include the first column (going to start token),
-- or the last rows (going from end tokens)

-- First and last rows/cols of the trans matrix are start and end
data Sequence s = Sequence {
    trans :: Trans
  , stateIxs :: V.Vector s
  } deriving Show

emptySequence :: Sequence s
emptySequence = Sequence {
    trans = M.fromRows (M.singVec (M.singVec 1))
  , stateIxs = V.empty
  }

getTrans :: Sequence s -> Trans
getTrans = addFixedEndRow . addStartColumn . collapseEnds . trans

addFixedEndRow :: Trans -> Trans
addFixedEndRow trans = appendRow (onehotVector (M.width trans) (M.width trans)) trans

collapseEnds :: Trans -> Trans
collapseEnds trans = M.hconcat [main, flatEnds]
  where (main, ends) = splitEnds trans
        flatEnds = setWidth 1 $ M.mapOnRows (M.singVec . sum) ends

type TransWithEndTransitions = (Trans, Int)

{-

SEXY ALERT

Here's how to implement the geometric distribution:

take the distributeEnds matrix between m and (m `eitherOr (1-p)` emptySeq),
and add it to m's transition matrix.

By adding, you're basically identifying the states between the two copies, thereby transitioning itself.
-}

{-
for andThen, feed distributeEnds seqA's [endsStart, endsTrans] and all of seqB's trans. it can then just fit in the upper right, the indices should work out.
-}

ds = deterministicSequence . V.fromList

deterministicSequence :: V.Vector s -> Sequence s
deterministicSequence states = Sequence {
    trans = M.idMx (V.length states + 1)
  , stateIxs = states
  }

andThen :: Sequence s -> Sequence s -> Sequence s
andThen seqA seqB = Sequence {
    trans = trans'
  , stateIxs = stateIxs'
  }
  where stateIxs' = stateIxs seqA <> stateIxs seqB

        transA = trans seqA
        transB = trans seqB
        (mainA, endsA) = splitEnds transA
        (_, nonstartB) = splitStart transB
        transition = transB `distributeEnds` endsA
        rightLen = max (M.width transition) (M.width transB)
        lowerLeft = M.zeroMx (M.height nonstartB, M.width mainA)
        trans' = M.blockMx [ [mainA, setWidth rightLen transition]
                           , [lowerLeft, setWidth rightLen nonstartB] ]

eitherOr :: Prob -> Sequence s -> Sequence s -> Sequence s
eitherOr p a b = Sequence {
    trans = joinTransTokens (start, mainTrans, startEnds, ends)
  , stateIxs = stateIxs'
  }
  where stateIxs' = stateIxs a <> stateIxs b

        (startA, mainTransA, startEndsA, endTransA) = splitTransTokens $ trans a
        (startB, mainTransB, startEndsB, endTransB) = splitTransTokens $ trans b

        start = ((* p) <$> startA) <> ((* (1 - p)) <$> startB)

        startEnds = ((* p) <$> startEndsA) + ((* (1 - p)) <$> startEndsB)

        endLen = max (M.width endTransA) (M.width endTransB)
        ends = M.vconcat [setWidth endLen endTransA, setWidth endLen endTransB]

        mainTrans = mainTransA `diagConcat` mainTransB

distributeEnds :: Trans -> Trans -> Trans
distributeEnds trans = mapRows (distributeEndDist trans)

distributeEndDist :: Trans -> Dist -> Dist
distributeEndDist trans = (`M.row` 1) . transStepDist trans

nStates :: Trans -> Int
nStates = pred . M.height

nEnds :: Trans -> Int
nEnds m = (M.width m) - (nStates m)

maxUsedEnd :: Trans -> Int
maxUsedEnd m = maybe 1 snd
               . find (M.isNotZeroVec . fst)
               . map (\ix -> (M.col m ix, ix - nStates m))
               $ [M.width m .. nStates m + 1]

splitTransTokens :: Trans -> (Dist, Trans, Dist, Trans)
splitTransTokens trans = (mainStart, mainTrans, endsStart, endsTrans)
  where (main, ends) = splitColsAt (nStates trans) trans
        (mainStart, mainTrans) = M.popRow 1 main
        (endsStart, endsTrans) = M.popRow 1 ends

splitEnds :: Trans -> (Trans, Trans)
splitEnds trans = splitColsAt (nStates trans) trans

splitStart :: Trans -> (Dist, Trans)
splitStart = M.popRow 1

joinTransTokens :: (Dist, Trans, Dist, Trans) -> Trans
joinTransTokens (mainStart, mainTrans, endsStart, endsTrans) =
  M.hconcat [ (prependRow mainStart mainTrans)
            , (prependRow endsStart endsTrans)
            ]

addEndTransitions :: Trans -> TransWithEndTransitions
addEndTransitions m = (addStartColumn $ M.vconcat [m, endTransitions], r)
  where (r, c) = M.dims m
        (_, endTransitions) = splitRowsAt r $ forwardDiagonal c

addStartColumn :: Trans -> Trans
addStartColumn trans = prependCol (M.zeroVec (M.height trans)) trans

removeStartColumn :: Trans -> Trans
removeStartColumn = M.delCol 1

addExtraEndTransitions :: Int -> TransWithEndTransitions -> TransWithEndTransitions
addExtraEndTransitions n (m, r) = (diagConcat (forwardDiagonal n) m, r)

addNEndTransitions :: Int -> Trans -> TransWithEndTransitions
addNEndTransitions n m = (if extraN > 0
                          then addExtraEndTransitions extraN
                          else id)
                         . addEndTransitions
                         $ m
  where (r, c) = M.dims m
        normalEnds = c - r
        extraN = n - normalEnds

removeEndTransitions :: TransWithEndTransitions -> Trans
removeEndTransitions (m, r) = removeStartColumn . fst $ splitRowsAt r m

-- second one should be constant
transStep :: Trans -> Trans -> Trans
transStep m = (`M.mul` mWithExtra)
  where (mWithExtra, _) = addEndTransitions m

transNSteps :: Trans -> Int -> Trans
transNSteps m 0 = M.idMx (M.width m)
transNSteps m n = (!! (n - 1)) . iterate (`transStep` m) $ m

transStepDist :: Trans -> M.SparseVector Prob -> Trans
transStepDist m dist = sum $ (\(ix, p) -> (p *) <$> transNSteps m ix) <$> M.vecToAssocList dist
