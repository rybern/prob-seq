{-# LANGUAGE TupleSections, BangPatterns, RecordWildCards, ViewPatterns #-}
module SNP where

import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Maybe
import Sequence
import MinION

type NT = Char

data Site a = Site {
    pos :: Int
  , alleles :: [(a, Prob)]
  , leftFlank :: [a]
  , rightFlank :: [a]
  } deriving Show

data SNPCallerRegion a = SNP [(a, Prob)]
                       | Flank [a]
                       | Noise Int
                       deriving Show

regionProbSeq :: [a] -> SNPCallerRegion a -> ProbSeq [a]
regionProbSeq keyOrder (SNP nts) = finiteDistOver . map (\(nt, p) -> (state [nt], p)) $ nts
regionProbSeq keyOrder (Flank nts) = series . map (state . return) $ nts
regionProbSeq keyOrder (Noise expected) = geometricRepeat (1 / fromIntegral expected) noise
  where noise = uniformDistOver (map (state . return) keyOrder)

regionsProbSeq :: [a] -> [SNPCallerRegion a] -> ProbSeq [a]
regionsProbSeq keyOrder = series . map (regionProbSeq keyOrder)

-- TODO: estimate starting and ending positions
snpRegions :: [Site a] -> [SNPCallerRegion a]
snpRegions sites = let end = last sites
                       first = head sites
                   in flankBetween (pos first - 20) Nothing (Just $ leftFlank first) (pos first)
                      ++ go sites
                      ++ flankBetween (pos end) (Just $ rightFlank end) Nothing (pos end + 20)
  where go (a:b:rest) = siteSNP a : siteFlankBetween a b ++ go (b:rest)
        go [a] = [siteSNP a]
        go [] = []

siteSNP :: Site a -> SNPCallerRegion a
siteSNP a = SNP (alleles a)

siteFlankBetween :: Site a -> Site a -> [SNPCallerRegion a]
siteFlankBetween a b = flankBetween (pos a) (Just $ rightFlank a) (Just $ leftFlank b) (pos b)

  -- assume a is earlier than b!
flankBetween :: Int -> Maybe [a] -> Maybe [a] -> Int -> [SNPCallerRegion a]
flankBetween aLoc maybeAFlank maybeBFlank bLoc = regions
  where regions = if noiseDist > 0
                  then catMaybes [Flank <$> maybeAFlank, Just $ Noise noiseDist, Flank <$> maybeBFlank]
                  else [Flank (take dist aFlank ++ drop (length bFlank - (dist - length aFlank)) bFlank)]
        aFlank = fromMaybe [] maybeAFlank
        bFlank = fromMaybe [] maybeBFlank
        dist = bLoc - aLoc - 1
        sumFlankLen = length aFlank + length bFlank
        noiseDist = dist - sumFlankLen

siteDistance :: Site a -> Site a -> Int
siteDistance a b = pos b - pos a - 1

ntComplement :: NT -> NT
ntComplement 'A' = 'T'
ntComplement 'T' = 'A'
ntComplement 'G' = 'C'
ntComplement 'C' = 'G'

siteComplement :: Site NT -> Site NT
siteComplement s@(Site {..}) = s {
    alleles = map (\(n, p) -> (ntComplement n, p)) alleles
  , leftFlank = map ntComplement leftFlank
  , rightFlank = map ntComplement rightFlank
  }

snpsMatSeqRC :: [NT] -> [Site NT] -> (MatSeq [NT], [[Vector Int]])
snpsMatSeqRC keyOrder (map siteComplement -> sites) = (matSeq, ixPairs)
  where regions = snpRegions sites
        matSeq = buildMatSeq . minion . reverseSequence . regionsProbSeq keyOrder $ regions

        isSNP (SNP vars) = Just (length vars)
        isSNP _ = Nothing
        snpIxs = catMaybes . zipWith (\i m -> (,i) <$> m) [0..] . map isSNP $ regions

        tags = V.map snd . stateLabels $ matSeq
        tagSNP snp allele (StateTag 0 [StateTag _ [StateTag 0 seqStates]]) = flip any seqStates $ \seqState ->
          case seqState of
            (StateTag n [StateTag x _])  -> x == allele && n == snp
            --(StateTag 1 [StateTag 0 [StateTag n [StateTag x _]]]) -> x == allele && n == snp
            _ -> False
        tagSNP _ _ _ = False

        allIxs = V.enumFromTo 0 (V.length tags - 1)
        ixPairs = flip map snpIxs $ \(nAlleles, snp) ->
          flip map [0..nAlleles-1] $ \allele ->
          V.filter (\i -> tagSNP snp allele $ tags V.! i) allIxs


snpsMatSeq :: [a] -> [Site a] -> (MatSeq [a], [[Vector Int]])
snpsMatSeq keyOrder sites = (matSeq, ixPairs)
  where regions = snpRegions sites
        matSeq = buildMatSeq . minion . regionsProbSeq keyOrder $ regions

        isSNP (SNP vars) = Just (length vars)
        isSNP _ = Nothing
        snpIxs = catMaybes . zipWith (\i m -> (,i) <$> m) [0..] . map isSNP $ regions

        tags = V.map snd . stateLabels $ matSeq
        tagSNP snp allele (StateTag 0 [StateTag _ seqStates]) = flip any seqStates $ \seqState ->
          case seqState of
            (StateTag n [StateTag x _])  -> x == allele && n == snp
            --(StateTag 1 [StateTag 0 [StateTag n [StateTag x _]]]) -> x == allele && n == snp
            _ -> False
        tagSNP _ _ _ = False

        allIxs = V.enumFromTo 0 (V.length tags - 1)
        ixPairs = flip map snpIxs $ \(nAlleles, snp) ->
          flip map [0..nAlleles-1] $ \allele ->
          V.filter (\i -> tagSNP snp allele $ tags V.! i) allIxs


{-
snpMatSeq :: Site a -> (MatSeq [a], Vector Int, Vector Int)
snpMatSeq (Site {..}) = (matSeq, refIxs, altIxs)
  where maf = snd $ alleles !! 1
        alt = fst $ alleles !! 1
        ref = fst $ alleles !! 0
        matSeq = buildMatSeq . minion $
          series [noise, series [left, eitherOr maf altState refState, right] , noise]
        altState = state [alt]
        refState = state [ref]
        left = series . map (state . return) $ leftFlank
        right = series . map (state . return) $ rightFlank
        noise = geometricRepeat 0.99 (uniformDistOver (map state keyOrder))

        tags = V.map snd . stateLabels $ matSeq
        tagSNP snp (StateTag 0 [StateTag 0 seqStates]) = flip any seqStates $ \seqState ->
          case seqState of
            (StateTag 1 [StateTag 1 [StateTag x _]]) -> x == snp
            _ -> False
        tagSNP _ _ = False

        allIxs = V.enumFromTo 0 (V.length tags - 1)
        altIxs = V.filter (\i -> tagSNP 0 $ tags V.! i) allIxs
        refIxs = V.filter (\i -> tagSNP 1 $ tags V.! i) allIxs
-}
