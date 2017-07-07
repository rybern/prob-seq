{-# LANGUAGE TupleSections, RecordWildCards, DeriveTraversable, DeriveFunctor, ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
module Sequence.ASTFix where

import Sequence.Matrix.Operations
import Sequence.Matrix.Sampling
import Sequence.Matrix.Types
import Sequence.ProbTree
import Control.Monad.Random
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe
import Data.Fix
import Data.Foldable
import Data.Monoid
import Unsafe.Coerce

-- for testing
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Control.Monad.Trans.Either
import Sequence.Matrix.Emissions

data CoreConstructor' a s t =
    CEmptySequence
  | CDeterministicSequence (Vector s)
  | CSkip Int
  | CMatrixForm (MatSeq s)
  | CEitherOr Prob t t
  | CAndThen t t
  | CIntersperse t t
  | CReverseSequence t
  | CCollapse Int t
  | CGeometricRepeat Prob t
  | CId a
  deriving (Show, Functor)

type CoreConstructor s t = CoreConstructor' t s t
type CoreProbSeq a s = Fix (CoreConstructor' a s)

newtype CoreProbSeq' s a = CoreProbSeq' (Fix (CoreConstructor' a s))
unCoreProbSeq (CoreProbSeq' a) = a

coreConstrMapA :: (a -> b) -> CoreConstructor' a s t -> CoreConstructor' b s t
coreConstrMapA f (CId a) = (CId (f a))
coreConstrMapA f c = unsafeCoerce c -- to avoid enumerating constructors without a

instance Functor (CoreProbSeq' s) where
  fmap f (CoreProbSeq' (Fix c)) = CoreProbSeq' . Fix . fmap g . coreConstrMapA f $ c
    where g = (unCoreProbSeq . fmap f . CoreProbSeq')

data Constructor s t =
    EmptySequence
  | DeterministicSequence (Vector s)
  | Skip Int
  | MatrixForm (MatSeq s)
  | EitherOr Prob t t
  | AndThen t t
  | Possibly Prob t
  | UniformDistOver [t]
  | FiniteDistOver [(t, Prob)]
  | GeometricRepeat Prob t
  | FiniteDistRepeat [Prob] t
  | UniformDistRepeat Int t
  | ReverseSequence t
  | Collapse Int t
  | SkipDist [Prob] t
  deriving (Show, Functor, Foldable, Traversable)

type ProbSeq s = Fix (Constructor s)

data ConstructorWith s a t = ConstructorWith {
    with :: a
  , constructor :: Constructor s t
  } deriving (Show, Functor, Foldable, Traversable)

newtype ProbSeqWith s a = ProbSeqWith (Fix (ConstructorWith s a))
  deriving Show
unProbSeqWith (ProbSeqWith x) = x

instance Functor (ProbSeqWith s) where
  fmap f (ProbSeqWith (Fix constr)) = ProbSeqWith . Fix $
    constr {
      with = f (with constr)
    , constructor = (unProbSeqWith . fmap f . ProbSeqWith) <$> (constructor constr)
    }

instance Foldable (ProbSeqWith s) where
  foldMap f (ProbSeqWith (Fix constr)) = foldMap (f . with . unFix) constr

instance Traversable (ProbSeqWith s) where
  traverse f (ProbSeqWith (Fix constr)) =
    (ProbSeqWith . Fix) <$> (ConstructorWith <$> with' <*> constructor')
    where constructor' = traverse ((unProbSeqWith <$>) . traverse f . ProbSeqWith) (constructor constr)
          with' = f (with constr)

mapWith :: ((ConstructorWith s a a) -> b)
        -> ProbSeqWith s a
        -> ProbSeqWith s b
mapWith cf (ProbSeqWith (Fix constr)) =
  let constr' = constr { with = cf (fmap (with . unFix) constr) }
  in ProbSeqWith . Fix $ constr' {
      constructor = (unProbSeqWith . mapWith cf . ProbSeqWith) <$> constructor constr'
    }

toCC = undefined

toC :: Constructor s a -> CoreProbSeq a s
toC = fix toCC

constrToCore :: CoreConstructor' a s (Constructor s a)
             -> CoreProbSeq a s
constrToCore = Fix . fmap toCore

cid :: a -> CoreProbSeq a s
cid = Fix . CId

coreFiniteDistOver :: [(CoreProbSeq a s, Prob)] -> CoreProbSeq a s
coreFiniteDistOver [(a, _)] = a
coreFiniteDistOver ((a, p) : rest) = Fix $ CEitherOr p a $
  coreFiniteDistOver (map (\(a', p') -> (a', p' / (1 - p))) rest)

toCore :: Constructor s a
       -> CoreProbSeq a s
toCore EmptySequence = Fix $ CEmptySequence
toCore (DeterministicSequence v) = Fix $ CDeterministicSequence v
toCore (MatrixForm v) = Fix $ CMatrixForm v
toCore (Skip n) = Fix $ CSkip n
toCore (EitherOr p a1 a2) = Fix $ CEitherOr p (cid a1) (cid a2)
toCore (AndThen a1 a2) = Fix $ CAndThen (cid a1) (cid a2)
toCore (ReverseSequence a1) = Fix $ CReverseSequence (cid a1)
toCore (GeometricRepeat p a1) = Fix $ CGeometricRepeat p (cid a1)
toCore (Collapse n a1) = Fix $ CCollapse n (cid a1)
toCore (SkipDist dist a1) =
  let skipSeq = coreFiniteDistOver $ zip (Fix CEmptySequence : map (Fix . CSkip) [1..]) dist
  in Fix $ CIntersperse skipSeq (cid a1)
toCore (UniformDistOver seqs) =
  let uniform = recip . fromIntegral . length $ seqs
  in toCore $ FiniteDistOver $ map (\seq -> (seq, uniform)) seqs
toCore (FiniteDistOver as) = coreFiniteDistOver . map (\(a, p) -> (cid a, p)) $ as
toCore (UniformDistRepeat n s) =
  let uniform = recip . fromIntegral . succ $ n
  in toCore $ FiniteDistRepeat (replicate n uniform) s
toCore (FiniteDistRepeat [] _) = constrToCore $ CEmptySequence
toCore (FiniteDistRepeat (p:ps) a) = constrToCore $ CAndThen (Possibly (1-p) a) (FiniteDistRepeat ps a)
toCore (Possibly p a) = Fix $ CEitherOr p (cid a) (Fix $ CEmptySequence)

buildCoreConstructor :: CoreConstructor s (MatSeq s) -> MatSeq s
buildCoreConstructor CEmptySequence = emptySequence
buildCoreConstructor (CDeterministicSequence v) = deterministicSequence v
buildCoreConstructor (CId m) = m
buildCoreConstructor (CMatrixForm m) = m
buildCoreConstructor (CSkip n) = skip n
buildCoreConstructor (CEitherOr p s1 s2) = eitherOr p s1 s2
buildCoreConstructor (CAndThen s1 s2) = andThen s1 s2
buildCoreConstructor (CGeometricRepeat p s) = geometricRepeat p s
buildCoreConstructor (CReverseSequence s) = reverseSequence s
--buildCoreConstructor (CCollapse n s) = collapse n s

buildConstructor :: Constructor s (MatSeq s) -> MatSeq s
buildConstructor = cata buildCoreConstructor . toCore

buildMatSeq :: ProbSeq s -> MatSeq s
buildMatSeq = cata buildConstructor

buildMatSeqTree :: ProbSeq s -> ProbSeqWith s (MatSeq s)
buildMatSeqTree = ProbSeqWith . Fix . cata (\constr -> ConstructorWith {
                                                 with = buildConstructor (with <$> constr)
                                               , constructor = Fix <$> constr
                                               } )

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


-- sampleSeq hangs on sampling empty

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

uniformSampleFrom :: (MonadRandom m) => [a] -> m a
uniformSampleFrom = fromList . toUniform
  where toUniform xs =
          let uniform = 1 / (fromIntegral (length xs))
          in map (\a -> (a, uniform)) xs

nonuniformSampleFrom :: (MonadRandom m) => [(a, Prob)] -> m (a, Prob)
nonuniformSampleFrom = fromList . map (\(a, p) -> ((a, p), p))

-- there's some more structural work to be done to allow for non-exact checking

checkMatSeqExact :: (Eq s, MonadRandom m)
                 => (m ((Vector s, Int), Prob) -> m [((Vector s, Int), Prob)])
                 -> ConstructorWith s (MatSeq s) (MatSeq s)
                 -> m (Maybe (CompareError s))
checkMatSeqExact sampleM c@(ConstructorWith {..}) = do
  subtreeSamples <- sampleM $ sampleConstructor uniformSampleFrom constructor
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
  c <- checkProbSeqExact (replicateM 10) mTree
  return . listToMaybe $ getFailures c

{-
used twice here, thrice in sampling


** Exception: Control.Monad.Random.Class.fromList: empty list, or total weight = 0
CallStack (from HasCallStack):
  error, called at ./Control/Monad/Random/Class.hs:383:16 in MonadRandom-0.5.1-IY6HiWu00vQLARukQW3KMH:Control.Monad.Random.Class
-}

generateFailures :: IO (CompareError Char)
generateFailures = do
  runs <- sequence . repeat $ do
    putStr "'"
    generateFailure
  return . head . catMaybes $ runs

err :: IO ((Vector Char, Int), Prob)
err = sampleSeqWithProb vecUniformDist (skip 3)

example = Fix $ EitherOr 0.9
  (Fix $ EitherOr 0.9
   (Fix $ DeterministicSequence ['a', 'b', 'c'])
   (Fix $ EmptySequence))
  (Fix $ EmptySequence)


  -- reverse sequence is broken. test case:
  -- (ReverseSequence (Possibly 0.15 (DeterministicSequence (V.singleton 1))))

arbitraryProbSeq :: Arbitrary s => Prob -> Gen (ProbSeq s)
arbitraryProbSeq = anaM geometricArbitraryConstructor

geometricArbitraryConstructor :: (Arbitrary s) => Prob -> Gen (Constructor s Prob)
geometricArbitraryConstructor p = (const (0.60 * p) <$>) <$>
  frequency [ (round (p * 1000),       arbitraryBranchConstructor)
            , (round ((1 - p) * 1000), arbitraryLeafConstructor)]

arbitraryBranchConstructor :: (Arbitrary s) => Gen (Constructor s ())
arbitraryBranchConstructor = oneof $
  [
    arbitraryEitherOr
  , arbitraryAndThen
  --, arbitraryPossibly
  --, arbitraryUniformDistOver
  --, arbitraryFiniteDistOver
  --, arbitraryFiniteDistRepeat
  --, arbitraryUniformDistRepeat
  --, arbitraryReverseSequence
  ]

arbitraryLeafConstructor :: (Arbitrary s) => Gen (Constructor s ())
arbitraryLeafConstructor = oneof
  [
    return $ EmptySequence
  , arbitraryDeterministicSequence
  , arbitrarySkip
  ]

arbitrarySkip :: Arbitrary s => Gen (Constructor s ())
arbitrarySkip = do
  n <- choose (0, 3)
  return (Skip n)

arbitraryDeterministicSequence :: Arbitrary s => Gen (Constructor s ())
arbitraryDeterministicSequence = do
  --let n = 1
  n <- choose (0, 4)
  v <- V.replicateM n arbitrary
  return (DeterministicSequence v)

probGen :: Gen Prob
probGen = toRational <$> (choose (0, 1) :: Gen Double)

distGen :: Int -> Gen [Prob]
distGen n = do
  is <- replicateM n probGen
  return $ (/ sum is) <$> is

arbitraryEitherOr ::
  Arbitrary s => Gen (Constructor s ())
arbitraryEitherOr = do
  q <- probGen
  return (EitherOr q () ())

arbitraryAndThen ::
  Arbitrary s => Gen (Constructor s ())
arbitraryAndThen = do
  return (AndThen () ())

arbitraryPossibly ::
  Arbitrary s => Gen (Constructor s ())
arbitraryPossibly = do
  q <- probGen
  return (Possibly q ())

arbitraryUniformDistOver ::
  Arbitrary s => Gen (Constructor s ())
arbitraryUniformDistOver = do
  n <- choose (1, 5)
  return (UniformDistOver $ replicate n ())

arbitraryFiniteDistOver ::
  Arbitrary s => Gen (Constructor s ())
arbitraryFiniteDistOver = do
  n <- choose (1, 5)
  ps <- distGen n
  let constPairs = map ((), ) ps
  return (FiniteDistOver constPairs)

arbitraryFiniteDistRepeat ::
  Arbitrary s => Gen (Constructor s ())
arbitraryFiniteDistRepeat = do
  n <- choose (1, 4)
  (_:ps) <- distGen n
  return (FiniteDistRepeat ps ())

arbitraryUniformDistRepeat ::
  Arbitrary s => Gen (Constructor s ())
arbitraryUniformDistRepeat = do
  n <- choose (1, 4)
  return (UniformDistRepeat n ())

arbitraryReverseSequence ::
  Arbitrary s => Gen (Constructor s ())
arbitraryReverseSequence = do
  return (ReverseSequence ())
