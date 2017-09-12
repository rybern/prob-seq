module Sequence.Matrix.IO.Write where

import Sequence.Matrix.Types
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Monoid ((<>))

import Sequence.Matrix.IO.TransMatrix
import Sequence.Matrix.IO.StateLabels

writeMatSeqFile :: (Trans -> Trans)
                -> MatSeq String
                -> FilePath
                -> IO ()
writeMatSeqFile f seq fp = Text.writeFile fp txt
  where txt = Text.unlines $ showMatSeq f seq

writeMatSeq :: (Trans -> Trans) -> MatSeq String -> Text
writeMatSeq f = Text.unlines . showMatSeq f

showMatSeq :: (Trans -> Trans) -> MatSeq String -> [Text]
showMatSeq f seq = showStateLabels (stateLabels seq) <> showTrans (f $ trans seq)
