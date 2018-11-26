module SMoL.Matrix.Types
  (
    module SMoL.Types
  , MatSeq (..)
  , Dist (..)
  , Trans (..)
  , StateTag (..)
  , StateLabel (..)
  ) where

import SMoL.Types
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified SparseMatrix as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

type Dist = M.SparseVector
type Trans = M.SparseMatrix

{-
additional to current StateTag? StateTag encodes all of this,
  could technically just insert ID of parent into the level, and search for it
  but the only information that will be extracted is whether the tag is present and which value

IntMap : TagID -> value

so could do
  stateLabels :: Vector (s, IntMap Int, StateTag)
where the entries in IntMap are (TagID, value)
-}

data StateTag = StateTag Int [StateTag]
  deriving (Show, Read, Eq)

data StateLabel s = StateLabel {
    stateLabel :: s
  , stateTag :: StateTag
  , tagSet :: IntMap Int
  } deriving (Show, Eq)

-- First and last rows/cols of the trans matrix are start and end
data MatSeq s = MatSeq {
    trans :: Trans
  , stateLabels :: Vector (StateLabel s)
  } deriving Show

instance (Eq s) => Eq (MatSeq s) where
  s1 == s2 = (stateLabels s1 == stateLabels s2)
             && M.dims (trans s1) == M.dims (trans s2)
             && (M.toAssocList (trans s1)) == (M.toAssocList (trans s2))
