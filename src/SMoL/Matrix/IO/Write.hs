module SMoL.Matrix.IO.Write
  ( HideLabels (..)
  , DecimalProb (..)
  --, writeMatSeq
  , showMatSeq
  )
where

import SMoL.Matrix.Types
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Monoid ((<>))

import SMoL.Matrix.IO.TransMatrix
import SMoL.Matrix.IO.StateLabels

type HideLabels = Bool

showMatSeq :: (Trans -> Trans)
           -> HideLabels
           -> DecimalProb
           -> MatSeq String
           -> ByteString
showMatSeq f True decimalProbs seq = showTrans decimalProbs (f $ trans seq)
showMatSeq f False decimalProbs seq = showStateLabels (stateLabels seq) <> showTrans decimalProbs (f $ trans seq)
