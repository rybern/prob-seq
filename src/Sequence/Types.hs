module Sequence.Types where

import qualified Data.Vector as V
import qualified Math.LinearAlgebra.Sparse as M

-- First and last rows/cols of the trans matrix are start and end
data Sequence s = Sequence {
    trans :: Trans
  , stateLabels :: V.Vector s
  } deriving Show

instance (Eq s) => Eq (Sequence s) where
  s1 == s2 = (stateLabels s1 == stateLabels s2)
             && M.dims (trans s1) == M.dims (trans s2)
             && (M.toAssocList (trans s1)) == (M.toAssocList (trans s2))

type Prob = Double
type Dist = M.SparseVector Prob
type Trans = M.SparseMatrix Prob
