{-# LANGUAGE RecordWildCards #-}
module Sequence.Matrix.Operations.Geometric where

import Sequence.Matrix.Types
import Sequence.Matrix.SparseMatrixUtils
import Sequence.Matrix.ProbSeqMatrixUtils
import Data.Monoid ((<>))
import Data.List (find, nub)
import Data.Maybe (fromJust, fromMaybe)
import Control.Applicative (liftA2)
import Data.Vector (Vector)
import qualified Data.Vector as V
--import qualified Math.LinearAlgebra.Sparse as M
import qualified SparseMatrix as M

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Sequence.Matrix.Operations.AndThen
import Sequence.Matrix.Operations.Deterministic
import Sequence.Matrix.Operations.EitherOr

-- seems to work as expected, just need to line everything up

{-
Here's how to implement the geometric distribution:

take the distributeEnds matrix between m and (m `eitherOr (1-p)` emptySeq),
and add it to m's transition matrix.

By adding, you're basically identifying the states between the two copies, thereby transitioning itself.
-}

-- p is the probability of exiting the loop!
-- expectation 1-p / p
-- p = 1/(1+e)

geometricRepeat :: Prob -> MatSeq s -> MatSeq s
geometricRepeat p (MatSeq {..}) = possibly (1-p) $ MatSeq {
    trans = trans'
  , stateLabels = appendLabel 0 stateLabels
  }
  where (main, ends) = splitEnds trans

        -- do we need to iterate this somehow? what do we do with the ends after distribution, let them go?
        transition = trans `distributeEnds` ends

        trans' = M.hconcat [main, M.scale p ends] + M.scale (1-p) transition

        --rightLen = max (M.width transition) (M.width trans)
        --lowerLeft = M.zeroMx (M.height nonstartB, M.width mainA)
        --trans' = M.blockMx [ [mainA, setWidth rightLen transition]
                           --, [lowerLeft, setWidth rightLen nonstartB] ]
        --trans' = M.vconcat [ mainA `happend` setWidth rightLen transition
                           --, lowerLeft `happend` setWidth rightLen nonstartB ]
