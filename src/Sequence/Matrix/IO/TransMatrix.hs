{-# LANGUAGE OverloadedLists, ViewPatterns #-}
module Sequence.Matrix.IO.TransMatrix where

import qualified Math.LinearAlgebra.Sparse as M
import qualified Data.Vector as V
import Data.List
import Data.List.Split
import System.IO
import System.FilePath.Posix

import Sequence.Matrix.Types
import Sequence.Matrix.ProbSeqMatrixUtils

import Sequence.Matrix.Operations hiding (intersperse)

-- .st files should be r = c+1 with r0 startdist
-- .st files should be internal representation

-- getTrans isn't exactly right

test = andThen
  (deterministicSequence ["one", "two", "three"])
  (eitherOr 0.8
    (deterministicSequence ["four", "five", "six"])
    (reverseSequence
      (collapse (V.foldl1 (++)) 2
        (deterministicSequence ["seven", "eight", "nine"]))))

stTrans :: (MatSeq a -> M.SparseMatrix Prob)
stTrans = addFixedEndRow . collapseEnds . trans

writeSTFile :: (Show a) => FilePath -> MatSeq a -> IO ()
writeSTFile = writeSeqToFile stTrans "st"

writeSTPFile :: (Show a) => FilePath -> MatSeq a -> IO ()
writeSTPFile = writeSeqToFile trans "stp"

writeSeqToFile :: (Show a)
               => (MatSeq a -> M.SparseMatrix Prob)
               -> String
               -> FilePath
               -> MatSeq a
               -> IO ()
writeSeqToFile t extension fp seq = withFile dest WriteMode $ writeSeq t seq
  where dest = if takeExtension fp == extension
               then fp
               else fp `addExtension` extension

writeSeq :: (Show a)
         => (MatSeq a -> M.SparseMatrix Prob)
         -> MatSeq a
         -> Handle
         -> IO ()
writeSeq t seq h = do

  let (labels, tags) = V.unzip $ stateLabels seq

  -- write label header
  hPutStr h "# "
  mapM_ (hPutStr h) (intersperse "," . map show . V.toList $ labels)
  hPutStrLn h ""

  -- write tag header
  hPutStr h "# "
  mapM_ (hPutStr h) (intersperse "," . map show . V.toList $ tags)
  hPutStrLn h ""

  -- write matrix lines
  mapM_ (hPutStrLn h) . map showElem . tail . M.toAssocList $ matrix
  where matrix = t seq
        showElem ((r, c), val) = show (pred r) ++ " " ++ show (pred c) ++ " " ++ show (fromRational val :: Double)

readSTFile :: (Read a)
           => FilePath
           -> IO (MatSeq a)
readSTFile = readSeqFromFile (\t s -> MatSeq (snd . M.popRow (M.height t) $ t) s)

readSTPFile :: (Read a)
            => FilePath
            -> IO (MatSeq a)
readSTPFile = readSeqFromFile MatSeq

-- should probably use attoparsec..
readSeqFromFile :: (Read a)
                => (M.SparseMatrix Prob -> V.Vector (a, StateTag) -> MatSeq a)
                -> FilePath
                -> IO (MatSeq a)
readSeqFromFile seqFn fp = do
  ((_:labelHeader):(_:tagHeader):matLines) <- lines <$> readFile fp

  putStrLn labelHeader
  putStrLn tagHeader

  let lineAssocList (words -> [r, c, val]) = ((succ (read r), succ (read c)), read val)
      matrix = M.fromAssocList $ map lineAssocList matLines
      labels = V.map read . V.fromList $ splitOn "," labelHeader
      tags = V.map read . V.fromList $ splitOn "," tagHeader
      stateLabels = V.zip labels tags

  return $ seqFn matrix stateLabels
