module MinION where

import Sequence
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List

data StateTree a = Collapsed [StateTree a]
                 | Alone a

keyOrder :: [String]
keyOrder = ["A", "C", "G", "T"]

joinState :: Vector String -> String
joinState = (\s -> "(" ++ s ++ ")") . intercalate "," . V.toList

minion :: ProbSeq a -> ProbSeq a
minion = skipDist [0.4, 0.25, 0.15, 0.1, 0.1]
       . collapse undefined joinState 5
