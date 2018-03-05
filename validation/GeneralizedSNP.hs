{-# LANGUAGE DeriveFunctor, TupleSections, BangPatterns, RecordWildCards, ViewPatterns #-}
module GeneralizedSNP
  ( genSite
  , genMatSeq
  , genMatIxs
  , specifyGenMatSeq
  , specifyGenMatSeqNT
  , GenSNPState (..)
  ) where

import SNP
import Data.List
import Data.Function
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Sequence
import MinION

import Sequence.Matrix.ProbSeqMatrixUtils
import Sequence.Matrix.IO.StateLabels
import Data.Csv
import qualified Data.ByteString.Lazy.Char8 as BS
import Math.LinearAlgebra.Sparse.Matrix hiding (trans)

data GenSNPState = NoiseKey Int
                 | LeftFlank Int
                 | Ref
                 | Alt
                 | RightFlank Int
                 deriving (Eq, Show)

flankSize = 10
noiseSize = 4

genSite :: Site GenSNPState
genSite = Site {
    pos = 0
  , alleles = [(Ref, 0.5), (Alt, 0.5)]
  , leftFlank = map LeftFlank [0..flankSize-1]
  , rightFlank = map RightFlank [0..flankSize-1]
  }

specifySNPState :: [a] -> Site a -> GenSNPState -> a
specifySNPState keyOrder _ (NoiseKey i) = keyOrder !! i
specifySNPState _ site (LeftFlank i) = leftFlank site !! i
specifySNPState _ site Ref = fst $ alleles site !! 0
specifySNPState _ site Alt = fst $ alleles site !! 1
specifySNPState _ site (RightFlank i) = rightFlank site !! i

  -- roughly 30 seconds
genMatSeq :: MatSeq (StateTree GenSNPState)
(genMatSeq, genMatIxs@[[refIxs, altIxs]]) = snpsMatSeq Alone (map NoiseKey [0..noiseSize-1]) [genSite]

setStateContains matSeq nt =
  Set.fromList . map fst . filter (any (== nt) . fst . snd) . zip [1..] . V.toList . stateLabels $ matSeq

genMatSeqIxSets = let refSet = setStateContains genMatSeq Ref
                      altSet = setStateContains genMatSeq Alt
                  in (refSet, altSet, refSet `Set.union` altSet)

genMatSeqAlleleIxs :: [(Int, Int)]
genMatSeqAlleleIxs = allelePairs (genMatSeq, [[refIxs, altIxs]])

{-
You should only need to find the ix pairs once per genSite

test this by inspection of tags for genMatSeq
-}
specifyGenMatSeq :: MatSeq (StateTree GenSNPState)
                 -> (Set Int, Set Int, Set Int)
                 -> [a]
                 -> Site a
                 -> MatSeq (StateTree a)
specifyGenMatSeq genMs sets keyOrder site = updateProbs . updateStateLabels $ genMs
  where updateStateLabels = mapStates (specifySNPState keyOrder site <$>)
        updateProbs = updateSinkRatio sets (let ((_, p):_) = alleles site in p)

updateSinkRatio :: (Set Int, Set Int, Set Int)
                -> Prob -- ref prob
                -> MatSeq s
                -> MatSeq s
updateSinkRatio (refSet, altSet, eitherSet) p ms = ms { trans = mapWithIxs update (trans ms) }
  where update (from, to) val =
          if (from - 1) `Set.member` eitherSet
          then val
          else case (to `Set.member` refSet, to `Set.member` altSet) of
               (True, False) -> p * val * 2
               (False, True) -> (1-p) * val * 2
               (False, False) -> val
               (True, True) -> error $ "Index " ++ show to ++ " is in both altSet and refSet!"

-- mapWithIxs :: (Num a, Eq a) => ((M.Index, M.Index) -> a -> a) -> M.SparseMatrix a -> M.SparseMatrix a
-- use symmetry, don't need pairs
-- let f (p, q) (a, b) = (q * 2 * a, p * 2 * b)

allelePairs :: (MatSeq (StateTree GenSNPState), [[V.Vector Int]]) -> [(Int, Int)]
allelePairs (genMs, [[refIxs, altIxs]]) = ixPairs
  where ixStateTag ix = (snd . (stateLabels genMs V.!) $ ix, ix)
        ixStateTags = V.toList . V.map ixStateTag
        tagPairs = pairOff (tagPair `on` fst) (ixStateTags refIxs) (ixStateTags altIxs)
        ixPairs = map (\((_, i1), (_, i2)) -> (i1, i2)) tagPairs

tagPair :: StateTag -> StateTag -> Bool
tagPair (StateTag n1 [StateTag 0 []]) (StateTag n2 [StateTag 0 []]) = True
tagPair (StateTag _ []) _ = False
tagPair _ (StateTag _ []) = False
tagPair (StateTag n1 l1) (StateTag n2 l2) = n1 == n2 && and (zipWith tagPair l1 l2)

pairOff :: (Eq b) => (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
pairOff pred (a:as) bs = case find (pred a) bs of
  Nothing -> pairOff pred as bs
  Just b' -> (a, b') : pairOff pred as (delete b' bs)
pairOff pred [] bs = []

specifyGenMatSeqNT :: MatSeq (StateTree GenSNPState)
                   -> Site Char
                   -> (MatSeq [NT], [[Vector Int]])
specifyGenMatSeqNT genMatSeq site =
  ( mapStates ntTreeToString $ specifyGenMatSeq genMatSeq genMatSeqIxSets keyOrder site
  , genMatIxs)

exSite p = Site {
    pos = 0
  , leftFlank =  "abcdefghij"
  , alleles = [('0', p), ('1', 1-p)]
  , rightFlank = "ABCDEFGHIJ"
  }
site = exSite 0.35

check ms = putStrLn $ show (trans ms # (100, 100))

(ms, ixs) = specifyGenMatSeqNT genMatSeq site

cleanTrans :: Trans -> Trans
cleanTrans = addStartColumn . collapseEnds

-- writing this many tuples takes 11 seconds
n :: Int
n = 2807229
tuples = zip3 [1..n] [n,n-1..1] (map sqrt [1..fromIntegral n :: Float])
writeTuples fp = BS.writeFile fp . encode $ tuples
-- 6 seconds??????
writeStateLabels fp = BS.writeFile fp . showStateLabels . stateLabels $ ms


{-

testing code


testMatSeq :: (Joinable b, Eq b) => (a -> b) -> [a] -> [Site a] -> MatSeq b
testMatSeq toJoinable (map toJoinable -> keyOrder) (map (toJoinable <$>) -> sites) =
  buildMatSeq . minion . regionsProbSeq keyOrder $ snpRegions sites

testA :: MatSeq (StateTree GenSNPState)
testA = testMatSeq Alone (map NoiseKey [0..noiseSize-1]) [genSite]
testA' = mapStates (specifySNPState keyOrder site <$>) testA
testA'' = updateSinkRatio genMatSeqIxSets (let ((_, p):_) = alleles site in p) testA'
testA''' = mapStates ntTreeToString $ testA''

testC :: MatSeq [NT]
testC = fst $ specifyGenMatSeqNT site

testB :: MatSeq [NT]
testB = testMatSeq return keyOrder [site]

--specifyGenMatSeq

--(testA, aIxs) = specifyGenMatSeqNT (exSite 0.25)
--(testB, bIxs) = snpsNTMatSeq [exSite 0.35]

eqMat :: Trans
eqMat = (trans testA''' - trans testB)
eq = isZeroMx eqMat

eqMatList :: [((Int, Int), Prob)]
eqMatList = toAssocList eqMat

{-
setStateContains matSeq nt =
  Set.fromList . map fst . filter (any (== nt) . fst . snd) . zip [0..] . V.toList . stateLabels $ matSeq
set0 = setStateContains testA '0'
set1 = setStateContains testA '1'
-}

inSet nt =
  filter (\e -> Set.member (fromIntegral $ e - 1) (setStateContains testA nt)) . map (fst . fst) $ toAssocList eqMat

-}
