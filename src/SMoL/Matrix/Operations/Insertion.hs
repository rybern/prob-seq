module SMoL.Matrix.Operations.Insertion where

import SMoL.Matrix.Types
import SMoL.Matrix.SparseMatrixUtils
import SMoL.Matrix.ProbSeqMatrixUtils
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

afterState :: MatSeq s -> MatSeq s -> Int -> MatSeq s
afterState seqA delim ix = undefined


{-
(delete, insert, mutate, slips, etc) -> intersperse -> insertAfterState :: MatSeq -> MatSeq -> Int -> MatSeq
-}
