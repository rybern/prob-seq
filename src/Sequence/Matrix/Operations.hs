{-# LANGUAGE RecordWildCards #-}
module Sequence.Matrix.Operations
  (
    emptySequence
  , state
  , eitherOr
  , andThen
  , collapse
  , getNormalTrans
  , getNormalTransWithEnds
  , reverseSequence
  , skip
  , geometricRepeat
  , filterUnreachableStates
  , nStates
  , reachableSkips
  , intersperse
  ) where


import Sequence.Matrix.Operations.AndThen
import Sequence.Matrix.Operations.EitherOr
import Sequence.Matrix.Operations.Collapsing
import Sequence.Matrix.Operations.Deterministic
import Sequence.Matrix.Operations.Insertion
import Sequence.Matrix.Operations.Geometric
import Sequence.Matrix.Operations.Reverse
import Sequence.Matrix.Operations.Filtering
import Sequence.Matrix.Operations.Products
import Sequence.Matrix.ProbSeqMatrixUtils

intersperse = undefined
