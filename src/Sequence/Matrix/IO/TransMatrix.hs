module Sequence.Matrix.IO.TransMatrix where

import qualified Math.LinearAlgebra.Sparse as M
import qualified Data.Vector as V
import Data.List
import System.IO
import System.FilePath.Posix

import Sequence.Matrix.Operations
import Sequence.Matrix.Types
import Sequence.Matrix.ProbSeqMatrixUtils

-- .st files should be r = c+1 with r0 startdist
-- .st files should be internal representation

-- getTrans isn't exactly right

writeSTFile :: FilePath -> MatSeq Char -> IO ()
writeSTFile = writeSeqToFile (addFixedEndRow . collapseEnds . trans) "st"

writeSTPFile :: FilePath -> MatSeq Char -> IO ()
writeSTPFile = writeSeqToFile trans "stp"

writeSeqToFile :: -- (Show a)
                (MatSeq Char -> M.SparseMatrix Prob)
               -> String
               -> FilePath
               -> MatSeq Char
               -> IO ()
writeSeqToFile t extension fp seq = withFile (fp ++ "." ++ extension) WriteMode $ \h -> do
  {-
  -- write header
  hPutStr h "# "
  mapM_ (hPutStr h) (intersperse " " . map show . V.toList $ stateLabels seq)
  hPutStrLn h ""
  -}
  hPutStr h "# "
  hPutStr h (V.toList $ stateLabels seq)
  hPutStrLn h ""

  -- write matrix lines
  mapM_ (hPutStrLn h) . map showElem . tail . M.toAssocList $ matrix
  where matrix = t seq
        showElem ((r, c), val) = show (pred r) ++ " " ++ show (pred c) ++ " " ++ show (fromRational val :: Double)
        fp' = if takeExtension fp == extension
              then fp
              else fp `addExtension` extension


readSTFile :: -- (Read a)
            FilePath
           -> IO (MatSeq Char)
readSTFile = readSeqFromFile (\t s -> MatSeq (snd . M.popRow (M.height t) $ t) s)

readSTPFile :: -- (Read a)
           FilePath
           -> IO (MatSeq Char)
readSTPFile = readSeqFromFile MatSeq

-- should probably use attoparsec..
readSeqFromFile :: -- (Read a)
                (M.SparseMatrix Prob -> V.Vector Char -> MatSeq Char)
                -> FilePath
                -> IO (MatSeq Char)
readSeqFromFile seqFn fp = do
  ((_:header:_):matLines) <- map words . lines <$> readFile fp

  let assocList [r, c, val] = ((succ (read r), succ (read c)), read val)

  return $ seqFn
    (M.fromAssocList $ map assocList matLines)
    (V.fromList header)
    --(V.map read $ V.fromList header)
