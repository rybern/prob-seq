module Sequence
  (
    module X
  ) where

import Sequence.Constructors as X

import Sequence.Matrix as X

import Sequence.Matrix.Sampling as X
  ( sampleSeq
  , sampleSeqIxs
  , sampleSeqWithProb
  , vecUniformDist
  , vecDist
  , randToIO
  )

import Sequence.Matrix.Emissions as X

import Sequence.Matrix.IO as X

import Sequence.Matrix.ProbSeqMatrixUtils as X
  ( mapStates
  )

import Data.Fix as X
  ( Fix (..)
  )

import qualified Data.Vector as V
import Control.Monad

test :: IO ()
test = do
  let ms = buildMatSeq seq
  replicateM_ 10 $
    sampleSeq vecDist ms >>= print
  --print (trans ms)
  where seq = skipDist skipD . collapse undefined concat 4 . geometricRepeat 0.99 . uniformDistOver . map state $ ["G", "A", "T", "C"]
        states = map (\c -> state [c]) ['A'..'z']
        skipD = [0.3, 0.2, 0.2, 0.1, 0.1, 0.1]

test' :: IO (V.Vector String, Int)
test' = do
  let ms = buildMatSeq seq
  sampleSeq vecDist ms
  where --seq = collapse undefined concat 3 . series' $ map (`andThen` skipDist) states
        seq = collapse undefined concat 3 . geometricRepeat 0.99 . uniformDistOver $ take 4 states
        states = map (\c -> state [c]) ['a'..'z']
        skipDist = finiteDistOver $ zip (map skip [0..]) [0.3, 0.3, 0.3, 0.1]

{-
there are two modes for composing skips:
trimming the ends of the second operand so that they can't add,
or adding the second ends to the remaining first ends.

so far, andThen' and series' are the only operations that trim ends.
At minimum, we also need to add geometricRepeat', but we should
probably add the other repeats for completeness.

i'm concerned because i'm not clear on geometric's policies for ends anyway.
-}
