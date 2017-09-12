module Sequence.Matrix.IO.Write where

import Sequence.Matrix.Types
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Monoid ((<>))

import Sequence.Matrix.IO.TransMatrix
import Sequence.Matrix.IO.StateLabels

writeMatSeq :: (MatSeq String -> Trans)
            -> MatSeq String
            -> FilePath
            -> IO ()
writeMatSeq trans seq fp = Text.writeFile fp txt
  where txt = Text.unlines $ showMatSeq trans seq

showMatSeq :: (MatSeq String -> Trans) -> MatSeq String -> [Text]
showMatSeq trans seq = showStateLabels (stateLabels seq) <> showTrans (trans seq)
