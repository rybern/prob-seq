module Sequence.Operations where

import Sequence.Types
import Sequence.Utils
import Data.Monoid ((<>))
import Data.List (find)
import Control.Applicative (liftA2)
import qualified Data.Vector as V
import qualified Math.LinearAlgebra.Sparse as M
import Prelude hiding (product)

{- should probably split a lot of this into utils, and rename utils to matrix.utils -}

emptySequence :: Sequence s
emptySequence = Sequence {
    trans = M.fromRows (M.singVec (M.singVec 1))
  , stateLabels = V.empty
  }

getTrans :: Sequence s -> Trans
getTrans = addFixedEndRow . addStartColumn . collapseEnds . trans

addFixedEndRow :: Trans -> Trans
addFixedEndRow trans = appendRow (onehotVector (M.width trans) (M.width trans)) trans

collapseEnds :: Trans -> Trans
collapseEnds trans = M.hconcat [main, flatEnds]
  where (main, ends) = splitEnds trans
        flatEnds = setWidth 1 $ M.mapOnRows (M.singVec . sum) ends


ds = deterministicSequence . V.fromList

deterministicSequence :: V.Vector s -> Sequence s
deterministicSequence states = Sequence {
    trans = M.idMx (V.length states + 1)
  , stateLabels = states
  }

eitherOr :: Prob -> Sequence s -> Sequence s -> Sequence s
eitherOr p a b = Sequence {
    trans = joinTransTokens (start, mainTrans, startEnds, ends)
  , stateLabels = stateLabels'
  }
  where stateLabels' = stateLabels a <> stateLabels b

        (startA, mainTransA, startEndsA, endTransA) = splitTransTokens $ trans a
        (startB, mainTransB, startEndsB, endTransB) = splitTransTokens $ trans b

        start = ((* p) <$> startA) <> ((* (1 - p)) <$> startB)

        startEnds = ((* p) <$> startEndsA) + ((* (1 - p)) <$> startEndsB)

        endLen = max (M.width endTransA) (M.width endTransB)
        ends = M.vconcat [setWidth endLen endTransA, setWidth endLen endTransB]

        mainTrans = mainTransA `diagConcat` mainTransB

andThen :: Sequence s -> Sequence s -> Sequence s
andThen seqA seqB = Sequence {
    trans = trans'
  , stateLabels = stateLabels'
  }
  where stateLabels' = stateLabels seqA <> stateLabels seqB

        transA = trans seqA
        transB = trans seqB
        (mainA, endsA) = splitEnds transA
        (_, nonstartB) = splitStart transB
        transition = transB `distributeEnds` endsA
        rightLen = max (M.width transition) (M.width transB)
        lowerLeft = M.zeroMx (M.height nonstartB, M.width mainA)
        trans' = M.blockMx [ [mainA, setWidth rightLen transition]
                           , [lowerLeft, setWidth rightLen nonstartB] ]

instance Monoid (Sequence s) where
  mappend = andThen
  mempty = emptySequence

distributeEnds :: Trans -> Trans -> Trans
distributeEnds trans = mapRows (distributeEndDist trans)

distributeEndDist :: Trans -> Dist -> Dist
distributeEndDist trans = (`M.row` 1) . transStepDist1 trans

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

addStartColumn :: Trans -> Trans
addStartColumn trans = prependCol (M.zeroVec (M.height trans)) trans

type TransWithEndTransitions = (Trans, Int)

removeEndTransitions :: TransWithEndTransitions -> Trans
--removeEndTransitions (m, r) = M.delCol 1 . fst $ splitRowsAt r m
removeEndTransitions (m, r) = trimZeroCols . M.delCol 1 . fst $ splitRowsAt r m

addEndTransitions :: Int -> Trans -> TransWithEndTransitions
addEndTransitions minEnds m = (M.vconcat [addStartColumn $ m, endTransitions], r)
  where (r, c) = M.dims m
        (_, endTransitions) = splitRowsAt r $ forwardDiagonal (r + max (nEnds m) minEnds)

-- second one should be constant
transStep :: Trans -> Trans -> Trans
transStep m1 m2 = removeEndTransitions (m1' `M.mul` m2', max r1 r2)
  where maxEnds = max (nEnds m1) (nEnds m2)
        (m1', r1) = addEndTransitions maxEnds m1
        (m2', r2) = addEndTransitions maxEnds m2

repeatSteps :: Trans -> [Trans]
repeatSteps m = undefined

transNSteps :: Trans -> Int -> Trans
transNSteps m 0 = M.idMx (M.width m)
transNSteps m n = (!! (n - 1)) . iterate (`transStep` m) $ m

transStepDist :: Trans -> Dist -> Trans
transStepDist m dist = sum $ (\(ix, p) -> (p *) <$> transNSteps m (ix - 1)) <$> M.vecToAssocList dist

transStepDist1 :: Trans -> Dist -> Trans
transStepDist1 m dist = sum $ (\(ix, p) -> (p *) <$> transNSteps m ix) <$> M.vecToAssocList dist

test = deterministicSequence . V.fromList $ "apple"
test2 = eitherOr 0.5 test test
test2t = trans $ eitherOr 0.5 test test
test2' = transStepDist (trans test2) (tov [1])

tov ls = (M.vecFromAssocList (zip [1..] ls))

  {-

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

-}


mapStates :: (a -> b) -> Sequence a -> Sequence b
mapStates f seq = seq {stateLabels = V.map f (stateLabels seq)}

testcs = deterministicSequence (V.fromList "asdfb")
testcs1 = eitherOr 0.3 testcs testcs
testcs2 = andThen testcs testcs1
testc = collapse 2 testcs2

--collapse n seq = (tuples, startTrans', mainTrans', endsStart, endsTrans''', tail $ M.vecToAssocList (M.rows endsTrans), trans')
collapse :: Int -> Sequence a -> Sequence (V.Vector a)
collapse n seq = Sequence {
    trans = trans'
  , stateLabels = stateLabels'
  }
  where (mainStart, mainTrans, endsStart, endsTrans) = splitTransTokens (trans seq)

        states = V.fromList [0 .. nStates (trans seq)]
        stateOuts s = V.fromList . map (pred . fst) . tail . M.vecToAssocList $ M.row mainTrans (s + 1)
        tuples = tuplifyStateTransitions n states stateOuts
        tupleIxs = [1..V.length tuples]

        stateLabels' = V.map (V.map (stateLabels seq V.!)) tuples

        prob from to = mainTrans M.# (from + 1, to + 1)
        pathProb = fst
                   . V.foldl1 (\(p, prev) (_, next) -> (p * prob prev next, next))
                   . V.map (\s -> (1, s))

        tupleTrans :: Int -> Int -> Prob
        tupleTrans i1 i2 = prob (V.head t1) (V.head t2)
          where t1 = tuples V.! i1
                t2 = tuples V.! i2

        mainTrans' = M.fromAssocList
          [((succ i1, succ i2), tupleTrans i1 i2)
          | i1 <- [0..V.length tuples - 1]
          , i2 <- [0..V.length tuples - 1]]

        startTrans' = M.vecFromAssocList . concat $
          [ V.toList
            . V.imap (\i' tup -> (succ i', s * pathProb tup))
            . V.filter (\tup -> V.head tup + 1 == i)
            $ tuples
          | (i, s) <- tail $ M.vecToAssocList mainStart]

        endsTrans' = --M.fromRows . M.vecFromAssocList .
          concat
          [ V.toList
            . V.map (\(i', _) -> (i' + 1, r))
            . V.filter (\(i', tup) -> V.last tup + 1 == i)
            . V.zip (V.fromList [1..])
            $ tuples
          | (i, r) <- tail $ M.vecToAssocList (M.rows endsTrans)]

        endsTrans'' =
          concatMap (\(i, r) -> V.toList
                                . V.concat
                                . V.toList
                                . V.imap (\i' tup ->
                                             if V.last tup + 1 == i
                                             then V.singleton (i' + 1, r)
                                             else V.empty)
                                $ tuples)
          . tail
          . M.vecToAssocList
          . M.rows
          $ endsTrans

        endsTrans''' = foldl
          (\m (i, r) -> M.replaceRow r i m)
          (M.zeroMx (M.height mainTrans', M.width endsTrans))
          endsTrans''

        trans' = joinTransTokens (startTrans', mainTrans', endsStart, endsTrans''')

tuplifyStateTransitions :: Int
                        -> V.Vector Int
                        -> (Int -> V.Vector Int)
                        -> V.Vector (V.Vector Int)
tuplifyStateTransitions toCollapse states stateOuts = V.map V.fromList $ V.concatMap (groups toCollapse) states
  where groups :: Int -> Int -> V.Vector [Int]
        groups 1 s = V.singleton [s]
        groups n s = V.concatMap (V.map (s:) . groups (n - 1)) (stateOuts s)

possibly :: Prob -> Sequence s -> Sequence s
possibly p = eitherOr (1-p) emptySequence

uniformDistOver :: [Sequence s] -> Sequence s
uniformDistOver seqs = finiteDistOver $ zip seqs (repeat uniformDensity)
  where uniformDensity = 1 / fromIntegral (length seqs)

finiteDistOver :: [(Sequence s, Prob)] -> Sequence s
finiteDistOver [(seq, _)] = seq
finiteDistOver ((seq, p) : rest) = eitherOr p seq $
  finiteDistOver (map (\(s', p') -> (s', p' / (1 - p))) rest)

skipDist :: Dist
         -> Sequence s
         -> Sequence s
skipDist kernel seq = seq {
  trans = normalize $ transStepDist (trans seq) kernel
  }

-- seems to work as expected, just need to line everything up

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


-- geometricRepeat ps has probability 1-sum ps of emptySequence
--geometricRepeat :: Prob -> Sequence s -> Sequence s
geometricRepeat p s = ds
  where next = eitherOr p s emptySequence
        ds =  trans next `distributeEnds` (snd . splitEnds . trans $ s)

finiteDistRepeat :: [Prob] -> Sequence s -> Sequence s
finiteDistRepeat dist seq = next (dropoutDist dist')
  where dist' = 1-sum dist : dist
        next [_] = emptySequence
        next (p:ps) = eitherOr p emptySequence $ seq `andThen` next ps

uniformDistRepeat :: Int -> Sequence s -> Sequence s
uniformDistRepeat n = finiteDistRepeat (replicate n (1 / fromIntegral n))

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

reverseSequence :: Sequence s -> Sequence s
reverseSequence s = Sequence {
    trans = let m = getTrans s
            in snd . popCol 1 . snd . M.popRow (M.height m) $ reverseT m
  , stateLabels = V.reverse (stateLabels s)
  }


{-
the product in its current form is useless


testA = deterministicSequence (V.fromList [1, 2, 3])
testB = deterministicSequence (V.fromList [4, 5, 6])
test' = product testA testB

-- asymetrical, throw away second's ends
-- new ixs are b-major
-- product :: Sequence a -> Sequence b -> Sequence (a, b)
product seqA seqB = (stateLabels', mainStart, mainTrans, endsTrans, trans')
  -- Sequence {
    -- trans = trans'
  -- , stateLabels = stateLabels'
  -- }
  where stateLabels' = liftA2 (flip (,)) (stateLabels seqB) (stateLabels seqA)

        transA = trans seqA
        statesA = nStates transB
        (mainStartA, mainTransA, endsStartA, endsTransA) = splitTransTokens transA

        transB = trans seqB
        statesB = nStates transB
        (mainStartB, mainTransB) = splitStart . fst . splitEnds $ transB
        --(mainStartB, mainTransB, endsStartB, endsTransB) = splitTransTokens transB

        toPos aIx bIx = statesB * (bIx - 1) + aIx

        mainStart = M.vecFromAssocList $
          [(toPos aIx bIx, aVal * bVal)
          | (bIx, bVal) <- M.vecToAssocList mainStartB
          , (aIx, aVal) <- M.vecToAssocList mainStartA]

        mainTrans = M.fromAssocList $
          [((toPos ar br, toPos ac bc), aVal * bVal)
          | ((br, bc), bVal) <- M.toAssocList mainTransB
          , ((ar, ac), aVal) <- M.toAssocList mainTransA]

        endsTrans = M.vconcat . replicate statesB $ endsTransA

        trans' = joinTransTokens (mainStart, mainTrans, endsStartA, endsTrans)
        -- start: >>= product
        -- main: blocks
        -- ends: product??? should seqB ever end?
        -- startEnds: weighted sum???

        -- would be nice to delegate..

--productTrans :: Sequence a -> Sequence b -> Sequence a
--productTrans seqA = mapStates fst . product seqA

-}
