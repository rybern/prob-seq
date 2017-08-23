{-# LANGUAGE RecordWildCards, OverloadedStrings, OverloadedLists #-}
module Sequence.ASTFix where

import Sequence.Constructors
import Sequence.Types
import Sequence.Matrix
import Sequence.Matrix.Operations
import Sequence.Matrix.Sampling
import Sequence.Matrix.Emissions
import Sequence.Matrix.Types

import Control.Monad.Random
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe
import Data.Fix
import Data.Foldable
import Data.Monoid

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import ArbitraryConstructors

import Control.Exception

-- if there are any other subtree results that could produce the output,
-- enumerate all those subtree results and query them.
sampleConstructor :: (MonadRandom m, Eq s)
                  => ([((Vector s, Int), Prob)] -> m ((Vector s, Int), Prob))
                  -> Constructor s (MatSeq s)
                  -> m ((Vector s, Int), Prob)
sampleConstructor _ EmptySequence = return ((V.empty, 0), 1.0)
sampleConstructor _ (DeterministicSequence v) = return ((v, 0), 1.0)
sampleConstructor _ (Skip n) = return ((V.empty, n), 1.0)
sampleConstructor sampleList (EitherOr p m1 m2) = do
  (s1, m1s1) <- sampleSeqWithProb vecUniformDist m1
  (s2, m2s2) <- sampleSeqWithProb vecUniformDist m2

  let m1s2 = stateSequenceProbability s2 m1
      m2s1 = stateSequenceProbability s1 m2

  sampleList [ (s1, p * m1s1 + (1-p) * m2s1)
             , (s2, (1-p) * m2s2 + p * m1s2)]
sampleConstructor _ (AndThen m1 m2) = do
  (s1, sk1) <- sampleSeq vecUniformDist m1
  (s2, sk2) <- sampleSeq vecUniformDist m2
  let sk1Leftover = max 0 (V.length s2 - sk1)
      s = (s1 <> V.drop sk1 s2, sk1Leftover + sk2)

  return (s, thenGenerators m1 m2 s)
  {-
sampleConstructor sampleList (Possibly p m) = do
  ((s1, _), p1) <- sampleSeqWithProb vecUniformDist m
  if s1 == V.empty
    then return (V.empty, p1 * p + 1-p)
    else do
    let pEmpty = stateSequenceProbability V.empty m
    sampleList [ (s1, p * p1)
               , (V.empty, 1-p + p * pEmpty) ]
sampleConstructor sampleList (FiniteDistOver ms) = do
  let sample m = do
        ((seq, _), p) <- sampleSeqWithProb vecUniformDist m
        return (seq, p)
  seqs <- mapM (\(m, p) -> (\(s, q) -> (s, p * q)) <$> sample m) ms
  sampleList seqs
sampleConstructor sampleList (GeometricRepeat p m) = undefined
sampleConstructor sampleList (FiniteDistRepeat ps m) = undefined
sampleConstructor sampleList (UniformDistRepeat n m) = undefined
sampleConstructor _ (ReverseSequence m) = do
  ((v, _), p) <- sampleSeqWithProb vecUniformDist m
  return (V.reverse v, p)
sampleConstructor sampleList (Collapse n m) = undefined
sampleConstructor sampleList (SkipDist dist m) = undefined
-}

thenGenerators :: (Eq s) => MatSeq s -> MatSeq s -> (Vector s, Int) -> Prob
thenGenerators m1 m2 (seq, skip) = sum $ do
  (left, right) <- map (\n -> V.splitAt n seq) [0..V.length seq]

  skip1 <- reachableSkips (trans m1)

  let pLeft = stateSequenceProbability (left, skip1) m1
  guard $ pLeft > 0

  if right == V.empty
    then do skip2 <- reachableSkips (trans m2)
            let skipped = skip1 + skip2 - skip
            guard $ 0 <= skipped && skipped <= skip1
            let pRight = sequenceSuffixProbability skipped (V.empty, skip2) m2
            return $ pRight * pLeft
    else do let skip2 = skip
                pRight = sequenceSuffixProbability skip1 (right, skip2) m2
            return $ pRight * pLeft

data CompareError s = CompareError {
    path :: (Vector s, Int)
  , probMatrix :: Prob
  , probSubmatrices :: Prob
  , matrices :: ConstructorWith s (MatSeq s) (MatSeq s)
  , samples :: [((Vector s, Int), Prob)]
  } deriving Show

printCompareError :: (Show s) => CompareError s -> IO ()
printCompareError (CompareError {..}) = do
  print $ "Sampled path " ++ show path
  print $ "  Matrix found " ++ show probMatrix
  print $ "  Submatrices found " ++ show probSubmatrices
  print $ "  On constructor " ++ show (fmap (nStates . trans) . constructor $ matrices)

-- there's some more structural work to be done to allow for non-exact checking

checkMatSeqExact :: (Eq s, MonadRandom m)
                 => (m ((Vector s, Int), Prob) -> m [((Vector s, Int), Prob)])
                 -> ConstructorWith s (MatSeq s) (MatSeq s)
                 -> m (Maybe (CompareError s))
checkMatSeqExact sampleM c@(ConstructorWith {..}) = do
  subtreeSamples <- sampleM $ sampleConstructor uniformSampleElemFrom constructor
  return . getFirst . mconcat . map First . flip map subtreeSamples $ \(v, p) ->
    let p' = stateSequenceProbability v with
    in if p == p'
       then Nothing
       else Just $ CompareError {
        path = v
      , probMatrix = p'
      , probSubmatrices = p
      , matrices = c
      , samples = subtreeSamples
      }

checkProbSeqExact :: (Eq s, MonadRandom m)
                  => (m ((Vector s, Int), Prob) -> m [((Vector s, Int), Prob)])
                  -> ProbSeqWith s (MatSeq s)
                  -> m (ProbSeqWith s (Maybe (CompareError s)))
checkProbSeqExact sampleM = sequence . mapWith (checkMatSeqExact sampleM)

checkAnyFailures :: ProbSeqWith s (Maybe (CompareError s))
                 -> Bool
checkAnyFailures = any isJust

getFailures :: ProbSeqWith s (Maybe (CompareError s))
            -> [CompareError s]
getFailures = catMaybes . toList

generateFailure :: IO (Maybe (CompareError Char))
generateFailure = do
  t <- generate $ arbitraryProbSeq 0.9
  let mTree = buildMatSeqTree t
      checkExact = checkProbSeqExact (replicateM 10) mTree
      catchError :: (Show b) => b -> SomeException -> IO a
      catchError t e = do
        print "there was an exception! here's the tree:"
        print t
        return undefined
  c <- catch checkExact (catchError t)
  return . listToMaybe $ getFailures c

generateFailures :: IO (CompareError Char)
generateFailures = do
  runs <- sequence . repeat $ do
    putStr "."
    generateFailure
  return . head . catMaybes $ runs

tree1 =
  Fix (AndThen
        (Fix EmptySequence)
        (Fix (EitherOr
               0.5
               (Fix (AndThen
                      (Fix (Skip 3))
                      (Fix (DeterministicSequence ['a']))))
               (Fix (DeterministicSequence ['b', 'c', 'd', 'e'])))))

tree4 =
  Fix (AndThen
        (Fix EmptySequence)
        (Fix (EitherOr
               0.5
               (Fix (Skip 2))
               (Fix (DeterministicSequence ['b', 'c', 'd', 'e'])))))


-- no error
tree2 = Fix (((EitherOr 0.5 (Fix (AndThen (Fix (AndThen (Fix EmptySequence) (Fix (Skip 3)))) (Fix (DeterministicSequence ['\172'])))) (Fix (EitherOr 0.5 (Fix EmptySequence) (Fix (DeterministicSequence ['+', 'y', '$', 'S'])))))))

-- no error
tree3 = Fix (AndThen (Fix EmptySequence) (Fix (DeterministicSequence ['\172'])))

err :: (Eq s, MonadRandom m) => ProbSeq s -> m (Maybe (CompareError s))
err tree = do
  let mTree = buildMatSeqTree tree
  c <- checkProbSeqExact (replicateM 10) mTree
  return . listToMaybe $ getFailures c

{-
Fix (AndThen (Fix EmptySequence) (Fix (EitherOr (60049806767231 % 562949953421312) (Fix (AndThen (Fix (AndThen (Fix EmptySequence) (Fix (Skip 3)))) (Fix (DeterministicSequence "\172")))) (Fix (EitherOr (5401711115106913 % 9007199254740992) (Fix EmptySequence) (Fix (DeterministicSequence "+y$\US")))))))
-}
