module Sequence.Matrix.IO.Write
  ( HideLabels (..)
  , DecimalProb (..)
  , writeMatSeqFile
  , writeMatSeq
  , showMatSeq
  )
where

import Sequence.Matrix.Types
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Monoid ((<>))

import Sequence.Matrix.IO.TransMatrix
import Sequence.Matrix.IO.StateLabels

type HideLabels = Bool

writeMatSeqFile :: (Trans -> Trans)
                -> HideLabels
                -> DecimalProb
                -> MatSeq String
                -> FilePath
                -> IO ()
writeMatSeqFile f hideLabels decimalProbs seq fp = Text.writeFile fp txt
  where txt = Text.unlines $ showMatSeq f hideLabels decimalProbs seq

writeMatSeq :: (Trans -> Trans)
            -> HideLabels
            -> DecimalProb
            -> MatSeq String
            -> Text
writeMatSeq f hideLabels decimalProbs = Text.unlines . showMatSeq f hideLabels decimalProbs

showMatSeq :: (Trans -> Trans)
            -> HideLabels
            -> DecimalProb
           -> MatSeq String
           -> [Text]
showMatSeq f True decimalProbs seq = showTrans decimalProbs (f $ trans seq)
showMatSeq f False decimalProbs seq = showStateLabels (stateLabels seq) <> showTrans decimalProbs (f $ trans seq)
