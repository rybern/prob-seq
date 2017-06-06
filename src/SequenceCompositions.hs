module SequenceCompositions where

import Types
import Sequence
import MatrixUtils
import qualified Data.Vector as V
import qualified Math.LinearAlgebra.Sparse as M

possibly :: Prob -> Sequence s -> Sequence s
possibly p = eitherOr p emptySequence

uniformDistOver :: [Sequence s] -> Sequence s
uniformDistOver seqs = finiteDistOver $ zip seqs (repeat uniformDensity)
  where uniformDensity = 1 / (fromIntegral (length seqs))

finiteDistOver :: [(Sequence s, Prob)] -> Sequence s
finiteDistOver [(seq, _)] = seq
finiteDistOver ((seq, p) : rest) = eitherOr p seq $
  finiteDistOver (map (\(s', p') -> (s', p' / (1 - p))) rest)

convolve :: Dist
         -> Sequence s
         -> Sequence s
convolve kernel seq = seq {
  trans = (normalize $ transStepDist (trans seq) kernel)
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

test = geometricRepeat 0.9 (ds "1234")

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

