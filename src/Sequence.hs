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

test :: IO (V.Vector Int, Int)
test = sampleSeqIxs vecDist $ buildMatSeq seq
  where seq = series $ map (andThen skipDist) states
        states = map state [1..10]
        skipDist = finiteDistOver $ zip (map skip [1..]) [0, 0, 0, 1]
