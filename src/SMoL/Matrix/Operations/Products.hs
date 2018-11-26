module SMoL.Matrix.Operations.Products where

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

{-
the product in its current form is useless


testA = deterministicSequence (V.fromList [1, 2, 3])
testB = deterministicSequence (V.fromList [4, 5, 6])
test' = product testA testB

-- asymetrical, throw away second's ends
-- new ixs are b-major
-- product :: Sequence a -> Sequence b -> Sequence (a, b)
product seqA seqB = (stateLabels', mainStart, mainTrans, endsTrans, trans')
  -- Sequence {
    -- trans = trans'
  -- , stateLabels = stateLabels'
  -- }
  where stateLabels' = liftA2 (flip (,)) (stateLabels seqB) (stateLabels seqA)

        transA = trans seqA
        statesA = nStates transB
        (mainStartA, mainTransA, endsStartA, endsTransA) = splitTransTokens transA

        transB = trans seqB
        statesB = nStates transB
        (mainStartB, mainTransB) = splitStart . fst . splitEnds $ transB
        --(mainStartB, mainTransB, endsStartB, endsTransB) = splitTransTokens transB

        toPos aIx bIx = statesB * (bIx - 1) + aIx

        mainStart = M.vecFromAssocList $
          [(toPos aIx bIx, aVal * bVal)
          | (bIx, bVal) <- M.vecToAssocList mainStartB
          , (aIx, aVal) <- M.vecToAssocList mainStartA]

        mainTrans = M.fromAssocList $
          [((toPos ar br, toPos ac bc), aVal * bVal)
          | ((br, bc), bVal) <- M.toAssocList mainTransB
          , ((ar, ac), aVal) <- M.toAssocList mainTransA]

        endsTrans = M.vconcat . replicate statesB $ endsTransA

        trans' = joinTransTokens (mainStart, mainTrans, endsStartA, endsTrans)
        -- start: >>= product
        -- main: blocks
        -- ends: product??? should seqB ever end?
        -- startEnds: weighted sum???

        -- would be nice to delegate..

--productTrans :: Sequence a -> Sequence b -> Sequence a
--productTrans seqA = mapStates fst . product seqA

-}
