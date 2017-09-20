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
