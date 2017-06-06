module Types where

import qualified Math.LinearAlgebra.Sparse as M

type Prob = Double
type Dist = M.SparseVector Prob
type Trans = M.SparseMatrix Prob
