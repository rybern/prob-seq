{-# LANGUAGE RecordWildCards #-}
module SMoL.Matrix.Operations
  (
    emptySequence
  , state
  , eitherOr
  , andThen
  , andThen'
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
  , skipDist
  ) where


import SMoL.Matrix.Operations.AndThen
import SMoL.Matrix.Operations.EitherOr
import SMoL.Matrix.Operations.Collapsing
import SMoL.Matrix.Operations.Deterministic
import SMoL.Matrix.Operations.Insertion
import SMoL.Matrix.Operations.SkipDist
import SMoL.Matrix.Operations.Geometric
import SMoL.Matrix.Operations.Reverse
import SMoL.Matrix.Operations.Filtering
import SMoL.Matrix.Operations.Products
import SMoL.Matrix.ProbSeqMatrixUtils

intersperse = undefined
