module Sequence.Matrix.IO.TransMatrix where

import qualified Math.LinearAlgebra.Sparse as M
import qualified Data.Vector as V
import Data.List
import Data.List.Split
import System.IO
import System.FilePath.Posix

--import Sequence.Matrix.Operations
import Sequence.Matrix.Types
import Sequence.Matrix.ProbSeqMatrixUtils

-- .st files should be r = c+1 with r0 startdist
-- .st files should be internal representation

-- getTrans isn't exactly right

writeSTFile :: (Show a) => FilePath -> MatSeq a -> IO ()
writeSTFile = writeSeqToFile (addFixedEndRow . collapseEnds . trans) "st"

writeSTPFile :: FilePath -> MatSeq Char -> IO ()
writeSTPFile = writeSeqToFile trans "stp"

writeSeqToFile :: (Show a)
               => (MatSeq a -> M.SparseMatrix Prob)
               -> String
               -> FilePath
               -> MatSeq a
               -> IO ()
writeSeqToFile t extension fp seq = withFile (fp ++ "." ++ extension) WriteMode $ \h -> do

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
        fp' = if takeExtension fp == extension
              then fp
              else fp `addExtension` extension


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
  ((_:labelHeader:tagHeader:_):matLines) <- map words . lines <$> readFile fp

  let lineAssocList [r, c, val] = ((succ (read r), succ (read c)), read val)
      matrix = M.fromAssocList $ map lineAssocList matLines
      labels = V.map read . V.fromList $ splitOn "," labelHeader
      tags = V.map read . V.fromList $ splitOn "," tagHeader
      stateLabels = V.zip labels tags

  return $ seqFn matrix stateLabels
