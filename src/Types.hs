module Types where

import qualified Math.LinearAlgebra.Sparse as M
import qualified Data.Vector as V

type Prob = Double
type Trans = M.SparseMatrix Prob

newtype TokenedTrans = TokenedTrans Trans
  deriving Show

newtype FullTokenedTrans = FullTokenedTrans TokenedTrans
  deriving Show

-- First and last rows/cols of the trans matrix are start and end
data Sequence s = Sequence {
    trans :: FullTokenedTrans
  , stateIxs :: V.Vector s
  } deriving Show

transMatrix :: Sequence s -> Trans
transMatrix (Sequence { trans = (FullTokenedTrans (TokenedTrans trans))}) = trans
