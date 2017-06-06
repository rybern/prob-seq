module SparseTransFiles where

import qualified Math.LinearAlgebra.Sparse as M
import qualified Data.Vector as V
import Data.List
import Sequence
import Types
import MatrixUtils
import System.IO
import System.FilePath.Posix

-- .st files should be r = c+1 with r0 startdist
-- .st files should be internal representation

-- getTrans isn't exactly right

writeSTFile :: (Show s) => FilePath -> Sequence s -> IO ()
writeSTFile = writeSeqToFile (addFixedEndRow . collapseEnds . trans) "st"

writeSTPFile :: (Show s) => FilePath -> Sequence s -> IO ()
writeSTPFile = writeSeqToFile trans "stp"

writeSeqToFile :: (Show a)
               => (Sequence a -> M.SparseMatrix Prob)
               -> String
               -> FilePath
               -> Sequence a
               -> IO ()
writeSeqToFile t extension fp seq = withFile (fp ++ "." ++ extension) WriteMode $ \h -> do
  -- write header
  hPutStr h "# "
  mapM_ (hPutStr h) (intersperse " " . map show . V.toList $ stateIxs seq)
  hPutStrLn h ""

  -- write matrix lines
  mapM_ (hPutStrLn h) . map showElem . tail . M.toAssocList $ matrix
  where matrix = t seq
        showElem ((r, c), val) = show (pred r) ++ " " ++ show (pred c) ++ " " ++ show val


readSTFile :: (Read a)
           => FilePath
           -> IO (Sequence a)
readSTFile = readSeqFromFile (\t s -> Sequence (snd . M.popRow (M.height t) $ t) s)

readSTPFile :: (Read a)
           => FilePath
           -> IO (Sequence a)
readSTPFile = readSeqFromFile Sequence

-- should probably use attoparsec..
readSeqFromFile :: (Read a)
                => (M.SparseMatrix Prob -> V.Vector a -> Sequence a)
                -> FilePath
                -> IO (Sequence a)
readSeqFromFile seqFn fp = do
  ((_:header):matLines) <- map words . lines <$> readFile fp

  let assocList [r, c, val] = ((succ (read r), succ (read c)), read val)

  return $ seqFn
    (M.fromAssocList $ map assocList matLines)
    (V.map read $ V.fromList header)
