module TransitionComposition (
    Sequence
  , andThen
  , eitherOr
  , convolve
  , Prob
  , Trans
  , deterministicSequence
  , uniformDist
  , finiteDist
  ) where


import Types
import qualified Math.LinearAlgebra.Sparse as M
import qualified Data.Vector as V
import Data.Monoid
import Data.Function ((&))

uniformDist :: [Sequence s] -> Sequence s
uniformDist seqs = finiteDist $ zip seqs (repeat uniformDensity)
  where uniformDensity = 1 / (fromIntegral (length seqs))

finiteDist :: [(Sequence s, Prob)] -> Sequence s
finiteDist [(seq, _)] = seq
finiteDist ((seq, p) : rest) = eitherOr p seq $
  finiteDist (map (\(s', p') -> (s', p' / (1 - p))) rest)

andThen :: Sequence s -> Sequence s -> Sequence s
andThen a b = Sequence {
    trans = addFullTokens $ joinTokenedTrans (starts, ends, trans'')
  , stateIxs = states
  }
  where states = stateIxs a <> stateIxs b

        (aStart, aEnd, aTrans) = splitTokenedTrans . removeFullTokens $ trans a
        (bStart, bEnd, bTrans) = splitTokenedTrans . removeFullTokens $ trans b

        starts = aStart <> M.zeroVec (M.dim bStart - 1)
        ends = M.zeroVec (M.dim aEnd) <> bEnd

        trans' = diagConcat aTrans bTrans

        transitionRow p = M.zeroVec (M.dim aStart - 1) <> ((p *) <$> (M.setLength (M.dim bStart - 1) bStart))
        aEndNonzeros = M.vecToAssocList $ aEnd
        trans'' = foldl
          (\t (endingIndex, endingProb) ->
              M.updRow (+ transitionRow endingProb) endingIndex t)
          trans'
          aEndNonzeros

convolve :: M.SparseVector Prob
         -> Sequence s
         -> Sequence s
convolve kernel seq = seq {
  trans = let (FullTokenedTrans (TokenedTrans trans')) = trans seq
          in FullTokenedTrans (TokenedTrans (normalize $ transStepDist trans' kernel))
  }

eitherOr :: Prob -> Sequence s -> Sequence s -> Sequence s
eitherOr p a b = Sequence {
    trans = addFullTokens $ joinTokenedTrans (starts, ends, trans')
  , stateIxs = states
  }
  where (aStart, aEnd, aTrans) = splitTokenedTrans . removeFullTokens $ trans a
        (bStart, bEnd, bTrans) = splitTokenedTrans . removeFullTokens $ trans b

        bStart' = (* (1 - p)) <$> bStart
        aStart' = (* p) <$> aStart
        aLen = M.dim aStart'
        bLen = M.dim bStart'
        aStartToEnd = aStart' M.! aLen
        bStartToEnd = bStart' M.! bLen
        starts = (M.setLength (aLen - 1) aStart') <> (M.vecIns bStart' (bLen, (aStartToEnd + bStartToEnd)))

        states = stateIxs a <> stateIxs b
        ends = aEnd <> bEnd
        trans' = diagConcat aTrans bTrans

verifyTokenedTrans :: M.SparseMatrix Prob -> TokenedTrans
verifyTokenedTrans m = if and [ c == r
                              --, all (== 1.0) (sum <$> M.rows m)
                              ]
                       then TokenedTrans m
                       else error "bad trans"
  where (r, c) = M.dims m

joinTokenedTrans :: (M.SparseVector Prob, M.SparseVector Prob, M.SparseMatrix Prob) -> TokenedTrans
joinTokenedTrans (startDist, endDist, trans) = verifyTokenedTrans $
  prependRow startDist . appendCol endDist $ trans


splitTokenedTrans :: TokenedTrans -> (M.SparseVector Prob, M.SparseVector Prob, M.SparseMatrix Prob)
splitTokenedTrans (TokenedTrans m) = (startDist, endDist, m'')
  where (startDist, m') = M.popRow 1 m
        (endDist, m'') = popCol (M.width m') m'

deterministicSequence :: V.Vector s -> Sequence s
deterministicSequence states = Sequence {
    trans = addFullTokens . verifyTokenedTrans $ M.idMx (V.length states + 1)
  , stateIxs = states
  }

forwardDiagonal :: (Eq a, Num a) => Int -> M.SparseMatrix a
forwardDiagonal n = M.delRow 1 . M.idMx . succ $ n

popStartDist :: M.SparseMatrix Prob -> (M.SparseVector Prob, M.SparseMatrix Prob)
popStartDist = M.popRow 1

addFullTokens :: TokenedTrans -> FullTokenedTrans
addFullTokens (TokenedTrans m) = FullTokenedTrans . TokenedTrans $
  appendRow (onehotVector (c + 1) (c + 1)) . prependCol (M.zeroVec r) $ m
  where (r, c) = M.dims m

removeFullTokens :: FullTokenedTrans -> TokenedTrans
removeFullTokens (FullTokenedTrans (TokenedTrans m)) = TokenedTrans $
  M.delRowCol (M.height m) 1 m

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

onehotVector :: (Eq a, Fractional a) => M.Index -> Int -> M.SparseVector a
onehotVector hot len = M.vecIns (M.zeroVec len) (hot, 1.0)

-- useful later?
-- maybeBlocksMx :: (Eq a, Fractional a) => [[M.SparseMatrix a]] -> M.SparseMatrix a

diagConcat :: (Eq a, Fractional a) => M.SparseMatrix a -> M.SparseMatrix a -> M.SparseMatrix a
diagConcat a d = M.blockMx [ [a, blockB]
                           , [blockC, d] ]
  where (rA, cA) = M.dims a
        (rD, cD) = M.dims d
        blockB = M.zeroMx (rA, cD)
        blockC = M.zeroMx (rD, cA)

transConcat :: (Eq a, Fractional a)
            => M.SparseMatrix a -- top left
            -> M.SparseMatrix a -- bottom right
            -> M.SparseMatrix a -- transition
            -> M.SparseMatrix a
transConcat blockA blockD trans = M.blockMx [ [blockA, blockB]
                                            , [blockC, blockD] ]
  where (rA, cA) = M.dims blockA
        (rD, cD) = M.dims blockD
        (rT, cT) = M.dims trans
        blockC = M.zeroMx (rD, cA)

        blockB = M.blockMx [ [M.zeroMx (rA - rT, cT), M.zeroMx (rA - rT, cD - cT)]
                           , [trans                 , M.zeroMx (rT     , cD - cT)] ]

-- this bugs out:
-- testTrans = normalize $ M.setSize (10, 10) (forwardDiagonal 10 + M.idMx 10)

-- also, assoc adds a 0 element even if there's already a nonzero last element

testTrans = normalize . M.delCol 11 $ forwardDiagonal 10 + M.idMx 10

normalize = M.mapOnRows (\r -> (/ sum r) <$> r)

transStepDist :: Trans -> M.SparseVector Prob -> Trans
transStepDist m dist = sum $ (\(ix, p) -> (p *) <$> transNSteps m (pred ix)) <$> M.vecToAssocList dist

transNSteps :: Trans -> Int -> Trans
transNSteps m 0 = M.idMx (M.width m)
transNSteps m n = (!! pred n) . iterate (`M.mul` m) $ m
