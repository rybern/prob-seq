module SMoL.Matrix.IO
  ( readSTPFile
  , writeSTPFile
  , writeSTPHandle
  , readSTFile
  , writeSTFile
  , writeSTHandle
  , readMatSeq
  --, writeMatSeq
  ) where

import System.FilePath.Posix
import System.IO
--import qualified Math.LinearAlgebra.Sparse as M
import qualified SparseMatrix as M

import SMoL.Matrix.ProbSeqMatrixUtils
import SMoL.Matrix.Types
import SMoL.Matrix.IO.Read
import SMoL.Matrix.IO.Write

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

writeExtensionHandle :: (Trans -> Trans)
                     -> HideLabels
                     -> DecimalProb
                     -> MatSeq String
                     -> Handle
                     -> IO ()
writeExtensionHandle f hideLabels decimalProbs seq h =
  let txt = showMatSeq f hideLabels decimalProbs seq
  in BS.hPutStr h txt

maybeAddExtension :: String -> FilePath -> FilePath
maybeAddExtension extension fp =
  if takeExtension fp == extension
  then fp
  else fp `addExtension` extension

{- STP Files -}

readSTPFile :: FilePath
            -> IO (Either ParseError (MatSeq String))
readSTPFile = readMatSeqFile id

writeSTPHandle :: MatSeq String
               -> Handle
               -> IO ()
writeSTPHandle = writeExtensionHandle id False False

writeSTPFile :: MatSeq String
            -> FilePath
            -> IO ()
writeSTPFile seq fp = withFile fp WriteMode (writeSTPHandle seq)

{- ST Files -}

readSTFile :: FilePath
            -> IO (Either ParseError (MatSeq String))
readSTFile = readMatSeqFile unCleanTrans

writeSTHandle :: MatSeq String
              -> Handle
              -> IO ()
writeSTHandle = writeExtensionHandle cleanTrans False True

writeSTFile :: MatSeq String
            -> FilePath
            -> IO ()
writeSTFile seq fp = withFile fp WriteMode (writeSTHandle seq)
