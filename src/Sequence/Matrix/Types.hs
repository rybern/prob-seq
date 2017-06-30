module Sequence.Matrix.Types
  (
    module Sequence.Types
  , MatSeq (..)
  , Dist (..)
  , Trans (..)
  ) where

import Sequence.Types
import qualified Data.Vector as V
import qualified Math.LinearAlgebra.Sparse as M

type Dist = M.SparseVector Prob
type Trans = M.SparseMatrix Prob

-- First and last rows/cols of the trans matrix are start and end
data MatSeq s = MatSeq {
    trans :: Trans
  , stateLabels :: V.Vector s
  } deriving Show

instance (Eq s) => Eq (MatSeq s) where
  s1 == s2 = (stateLabels s1 == stateLabels s2)
             && M.dims (trans s1) == M.dims (trans s2)
             && (M.toAssocList (trans s1)) == (M.toAssocList (trans s2))
