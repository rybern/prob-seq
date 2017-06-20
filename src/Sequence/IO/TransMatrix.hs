module Sequence.IO.TransMatrix where

import qualified Math.LinearAlgebra.Sparse as M
import qualified Data.Vector as V
import Data.List
import System.IO
import System.FilePath.Posix

import Sequence.Operations
import Sequence.Types
import Sequence.Utils

-- .st files should be r = c+1 with r0 startdist
-- .st files should be internal representation

-- getTrans isn't exactly right

writeSTFile :: FilePath -> Sequence Char -> IO ()
writeSTFile = writeSeqToFile (addFixedEndRow . collapseEnds . trans) "st"

writeSTPFile :: FilePath -> Sequence Char -> IO ()
writeSTPFile = writeSeqToFile trans "stp"

writeSeqToFile :: -- (Show a)
                (Sequence Char -> M.SparseMatrix Prob)
               -> String
               -> FilePath
               -> Sequence Char
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
        showElem ((r, c), val) = show (pred r) ++ " " ++ show (pred c) ++ " " ++ show val


readSTFile :: -- (Read a)
            FilePath
           -> IO (Sequence Char)
readSTFile = readSeqFromFile (\t s -> Sequence (snd . M.popRow (M.height t) $ t) s)

readSTPFile :: -- (Read a)
           FilePath
           -> IO (Sequence Char)
readSTPFile = readSeqFromFile Sequence

-- should probably use attoparsec..
readSeqFromFile :: -- (Read a)
                (M.SparseMatrix Prob -> V.Vector Char -> Sequence Char)
                -> FilePath
                -> IO (Sequence Char)
readSeqFromFile seqFn fp = do
  ((_:header:_):matLines) <- map words . lines <$> readFile fp

  let assocList [r, c, val] = ((succ (read r), succ (read c)), read val)

  return $ seqFn
    (M.fromAssocList $ map assocList matLines)
    (V.fromList header)
    --(V.map read $ V.fromList header)
