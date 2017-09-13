module Main where

import qualified Data.Vector as V
import Data.Vector (Vector)
import Control.Monad
import System.Environment
import Data.List

import Sequence

main :: IO ()
main = sampleSequence 100
--main = do
  --n <- (read . head) <$> getArgs
  --writeSequence n




single = Fix $ DeterministicSequence (V.fromList $ map (\a -> [a]) "abb")

withSkip = Fix $ AndThen
  single
  (Fix $ Possibly 0.2 $ Fix $ FiniteDistRepeat [0.5, 0.5] (Fix $ Skip 1))

repeated = seqUniformLength 10 12 withSkip

str' = Fix $ Collapse undefined (\v -> "[" ++ (intercalate "," $ V.toList v) ++ "]") 2 repeated




writeSequence :: Int -> IO ()
writeSequence n = do
  let seq = lengthSeq n
      matSeq = buildMatSeq seq

  writeSTFile matSeq "test"

sampleSequence :: Int -> IO ()
sampleSequence n = do
  --let seq = Fix . DeterministicSequence . V.fromList $ replicate 1000 'c'
  --let seq = seqLength n (c 'a' `et` c 'b')
  let seq = seqLength n elementary

  a <- sample seq
  print a

et a b = Fix $ EitherOr 0.5 a b
at a b = Fix $ AndThen a b

c a = Fix $ DeterministicSequence (V.fromList a)

sample :: (Eq a) => ProbSeq a -> IO (Vector a, Int)
sample = randToIO . sampleSeq vecDist . buildMatSeq

lengthSeq :: Int -> ProbSeq String
lengthSeq n = seqLength n (uniformNT)

seqLength :: Int -> ProbSeq s -> ProbSeq s
seqLength n p = foldl1 (\a b -> Fix $ AndThen a b) $ replicate n p

uniformSeq :: Int -> ProbSeq String
uniformSeq n = seqUniformLength n (n + 1) elementary

elementary :: ProbSeq String
elementary = Fix $ AndThen
  (seqLength 5 uniformNT)
  (Fix $ UniformDistRepeat 4 (Fix $ Skip 1))

uniformNT :: ProbSeq String
uniformNT = (Fix . UniformDistOver . map (Fix . DeterministicSequence . V.singleton) $ ["G", "A", "T", "C"])

seqUniformLength :: Int -> Int -> ProbSeq s -> ProbSeq s
seqUniformLength min max s = Fix $ FiniteDistRepeat dist s
  where len = max - min
        prob = fromIntegral 1 / fromIntegral len
        dist = replicate min 0 ++ replicate len prob
