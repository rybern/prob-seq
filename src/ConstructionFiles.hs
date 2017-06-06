{-# LANGUAGE OverloadedStrings #-}
module ConstructionFiles where

import BuildConstructor
import SparseTransFiles
import SequenceCompositions
import Data.Text hiding (zip)
import Data.Text as Text (unlines)
import qualified Math.LinearAlgebra.Sparse as M
import qualified Data.Vector as V
import System.FilePath.Posix
import System.Exit
import Sequence

readSeqFile :: FilePath ->
               IO (Sequence Integer)
readSeqFile fp =
  case takeExtension fp of
    ".st" -> readSTFile fp
    ".stp" -> readSTPFile fp
    ".stb" -> runBuilderFile fp

runBuilderFile :: FilePath -> IO (Sequence Integer)
runBuilderFile fp = buildValueFromFileM constructors fp >>= \res ->
  case res of
    Left e -> die (show e)
    Right v -> eitherT (die . show) return v

runBuilderLines :: Text -> IO (Sequence Integer)
runBuilderLines txt =
  case buildValueM constructors txt of
    Left e -> die (show e)
    Right v -> eitherT (die . show) return v

constructors =
  [
    ("deterministic", SimpleListOfInt (C0 . deterministicSequence . V.fromList))
  , ("andThen", Composer (\seqA -> Composer (\seqB -> C0 $ andThen seqA seqB)))
  , ("eitherOr", SimpleFloat (\p -> Composer (\seqA -> Composer (\seqB -> C0 $ eitherOr p seqA seqB))))
  , ("emptySequence", C0 emptySequence)
  , ("reverse", Composer (C0 . reverseSequence))
  , ("possibly", SimpleFloat (\p -> Composer (\seq -> C0 $ possibly p seq)))
  , ("uniformDistOver", SimpleListOfValue (C0 . uniformDistOver))
  , ("finiteDistOver", SimpleListOfFloat (\ps ->
                                            SimpleListOfValue (\seqs ->
                                                                 C0 $ finiteDistOver (zip seqs ps))))
  , ("convolve", SimpleListOfFloat (\ps ->
                                      Composer (\seq ->
                                                  C0 $ convolve (M.vecFromAssocList (zip [1..] ps)) seq)))
  , ("uniformDistRepeat", SimpleInt (\n -> Composer (\seq -> C0 $ uniformDistRepeat (fromIntegral n) seq)))
  , ("finiteDistRepeat", SimpleListOfFloat (\ps ->
                                            Composer (\seq ->
                                                         C0 $ finiteDistRepeat ps seq)))
  , ("readFile", SimpleString (\fp -> Monadic (C0 <$> readSeqFile (unpack fp))))
  ]

testFile = Text.unlines
  [
    "seqA = deterministic \"abcd\""
  , "seqB = deterministic \"efg\""
  , "andThen seqA (eitherOr 0.5 emptySequence seqB)"
  ]

built = buildValueM constructors testFile
