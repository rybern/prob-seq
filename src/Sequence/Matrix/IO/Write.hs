module Sequence.Matrix.IO.Write
  ( HideLabels (..)
  , DecimalProb (..)
  --, writeMatSeq
  , showMatSeq
  )
where

import Sequence.Matrix.Types
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Monoid ((<>))

import Sequence.Matrix.IO.TransMatrix
import Sequence.Matrix.IO.StateLabels

type HideLabels = Bool

showMatSeq :: (Trans -> Trans)
           -> HideLabels
           -> DecimalProb
           -> MatSeq String
           -> ByteString
showMatSeq f True decimalProbs seq = showTrans decimalProbs (f $ trans seq)
showMatSeq f False decimalProbs seq = showStateLabels (stateLabels seq) <> showTrans decimalProbs (f $ trans seq)
