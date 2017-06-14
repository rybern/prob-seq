module SequenceCompositions where

import Types
import Sequence
import MatrixUtils
import Data.Monoid ((<>))
import Control.Applicative (liftA2)
import qualified Data.Vector as V
import qualified Math.LinearAlgebra.Sparse as M
import Prelude hiding (product)
mapStates :: (a -> b) -> Sequence a -> Sequence b
mapStates f seq = seq {stateIxs = V.map f (stateIxs seq)}

testcs = deterministicSequence (V.fromList "asdfb")
testcs1 = eitherOr 0.3 testcs testcs
testcs2 = andThen testcs testcs1
testc = collapse 2 testcs2

--collapse n seq = (tuples, startTrans', mainTrans', endsStart, endsTrans''', tail $ M.vecToAssocList (M.rows endsTrans), trans')
collapse :: Int -> Sequence a -> Sequence (V.Vector a)
collapse n seq = Sequence {
    trans = trans'
  , stateIxs = stateIxs'
  }
  where (mainStart, mainTrans, endsStart, endsTrans) = splitTransTokens (trans seq)

        states = V.fromList [0 .. nStates (trans seq)]
        stateOuts s = V.fromList . map (pred . fst) . tail . M.vecToAssocList $ M.row mainTrans (s + 1)
        tuples = tuplifyStateTransitions n states stateOuts
        tupleIxs = [1..V.length tuples]

        stateIxs' = V.map (V.map (stateIxs seq V.!)) tuples

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
possibly p = eitherOr p emptySequence

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

--geometricRepeat :: Prob -> Sequence s -> Sequence s
geometricRepeat p s = ds
  where next = eitherOr p s emptySequence
        ds =  trans next `distributeEnds` (snd . splitEnds . trans $ s)

finiteDistRepeat :: [Prob] -> Sequence s -> Sequence s
finiteDistRepeat dist seq = next (dropoutDist dist)
  where next [] = emptySequence
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

reverseT m = normalize . flip M.ins ((1,1), 0)
             . flip M.ins ((M.height m, M.width m), 1.0)
             . mapWithIxs bayes $ M.trans flipped
  where ps' = priors m
        ps = reverseVec ps'

        flipped = reverseRows . reverseCols $ m
        --flipped = reverseRows . reverseCols . snd . M.popRow (M.height m) $ m
        bayes (r, c) val = (ps M.! r) * val / (ps M.! c)

reverseSequence s = Sequence {
    trans = let m = getTrans s
            in snd . popCol 1 . snd . M.popRow (M.height m) $ reverseT m
  , stateIxs = V.reverse (stateIxs s)
  }


{-
the product in its current form is useless


testA = deterministicSequence (V.fromList [1, 2, 3])
testB = deterministicSequence (V.fromList [4, 5, 6])
test' = product testA testB

-- asymetrical, throw away second's ends
-- new ixs are b-major
-- product :: Sequence a -> Sequence b -> Sequence (a, b)
product seqA seqB = (stateIxs', mainStart, mainTrans, endsTrans, trans')
  -- Sequence {
    -- trans = trans'
  -- , stateIxs = stateIxs'
  -- }
  where stateIxs' = liftA2 (flip (,)) (stateIxs seqB) (stateIxs seqA)

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
