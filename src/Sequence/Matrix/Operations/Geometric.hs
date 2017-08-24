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
import qualified Math.LinearAlgebra.Sparse as M

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Sequence.Matrix.Operations.AndThen
import Sequence.Matrix.Operations.Deterministic
import Sequence.Matrix.Operations.EitherOr

-- seems to work as expected, just need to line everything up

{-

SEXY ALERT

Here's how to implement the geometric distribution:

take the distributeEnds matrix between m and (m `eitherOr (1-p)` emptySeq),
and add it to m's transition matrix.

By adding, you're basically identifying the states between the two copies, thereby transitioning itself.
-}


-- geometricRepeat ps has probability 1-sum ps of emptySequence
geometricRepeat :: Prob -> MatSeq s -> MatSeq s
geometricRepeat p s = undefined
  where next = eitherOr p s emptySequence
        ds = trans next `distributeEnds` (snd . splitEnds . trans $ s)
