module Sequence.Matrix.IO
  ( readSTPFile
  , writeSTPFile
  , readSTFile
  , writeSTFile
  , readMatSeq
  , writeMatSeq
  ) where

import System.FilePath.Posix
import qualified Math.LinearAlgebra.Sparse as M

import Sequence.Matrix.ProbSeqMatrixUtils
import Sequence.Matrix.Types
import Sequence.Matrix.IO.Read
import Sequence.Matrix.IO.Write

type Extension = String

writeExtensionFile :: Extension
                   -> (Trans -> Trans)
                   -> MatSeq String
                   -> FilePath
                   -> IO ()
writeExtensionFile ext f seq fp = writeMatSeqFile f seq (maybeAddExtension ext fp)

maybeAddExtension :: Extension -> FilePath -> FilePath
maybeAddExtension extension fp =
  if takeExtension fp == extension
  then fp
  else fp `addExtension` extension

{- STP Files -}

readSTPFile :: FilePath
            -> IO (Either ParseError (MatSeq String))
readSTPFile = readMatSeqFile id

writeSTPFile :: MatSeq String
            -> FilePath
            -> IO ()
writeSTPFile = writeExtensionFile ".stp" id

{- ST Files -}

readSTFile :: FilePath
            -> IO (Either ParseError (MatSeq String))
readSTFile = readMatSeqFile unCleanTrans

unCleanTrans :: Trans -> Trans
unCleanTrans t = (snd . M.popRow (M.height t) $ t)

writeSTFile :: MatSeq String
            -> FilePath
            -> IO ()
writeSTFile = writeExtensionFile ".st" cleanTrans

cleanTrans :: Trans -> Trans
cleanTrans = addFixedEndRow . collapseEnds
