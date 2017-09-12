{-# LANGUAGE OverloadedLists #-}
module Sequence.Matrix.IO.Read (readMatSeq) where

import qualified Data.Vector as V
import Data.Vector (Vector)

import Sequence.Matrix.Types

import Data.Text
import Data.Attoparsec.Text

import Sequence.Matrix.IO.TransMatrix
import Sequence.Matrix.IO.StateLabels

type ParseError = String

readMatSeq :: Text
           -> Either ParseError (MatSeq String)
readMatSeq = parseOnly parseMatSeq

parseMatSeq :: Parser (MatSeq String)
parseMatSeq = do
  stateLabels <- parseStateLabels
  endOfLine
  trans <- parseTrans

  return $ MatSeq trans stateLabels

{-
FOR TESTING:


import qualified Math.LinearAlgebra.Sparse as M
import Sequence.Matrix.Operations hiding (intersperse)
import Sequence.Matrix.IO.Write

testTrans :: MatSeq String -> Either String Trans
testTrans = parseOnly parseTrans . Data.Text.unlines . showTrans . trans

testStateLabels :: MatSeq String -> Either String (Vector (String, StateTag))
testStateLabels = parseOnly parseStateLabels . Data.Text.unlines . showStateLabels . stateLabels

testMatSeq :: MatSeq String -> Either String (MatSeq String)
testMatSeq = parseOnly parseMatSeq . Data.Text.unlines . showMatSeq trans

test = andThen
  (deterministicSequence ["one", "two", "three"])
  (eitherOr 0.8
    (deterministicSequence ["four", "five", "six"])
    (reverseSequence
      (collapse (V.foldl1 (++)) 2
        (deterministicSequence ["seven", "eight", "nine"]))))
-}
