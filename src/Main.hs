{-# LANGUAGE OverloadedLists #-}
module Main where

import Data.Vector
import Data.Vector as V
import Sampling
import Types
import TransitionComposition

{-
The potential complications come from not annotating/using structural information. Strategies for joining two sequences that attempt to use structural information will have to attempt to regain that structural information, and that will be complicated.

The primary loss of information comes from the fact that the distributions end with the End token, so you can't know if the state would have jumped past the end into 'End + 1', and so you can't know if it should transition deeper into the second sequence.

One way to avoid this problem is to avoid complicated joints. Each of the use cases could be modeled first using single-step sequences, then joining them, and finally convolving them to account for nanopore slippage.

One way to add back the complexity would be to carry around structural annotations for the indices - but this would eliminate cycles like geometric distributions.

Another way would be to carry additional End tokens: End1-EndN. It seems like this could work.
  -}

{-
Current todos:

Clean up the transition matrix definitions so that:
  - other modules don't have to unwrap shit to get the transmatrix (solved by transMatrix in types?)
  - edge cases are handled, like sequence ([[1]], []) is the identity sequence
-}

{-
Rewrite without the whole 'token wrapping', and with one start and some number of ends.
-}

deterministicSequence' = deterministicSequence . V.fromList
like = deterministicSequence' "I like "
apple = deterministicSequence' "apples"
banana = deterministicSequence' "bananas"
question = deterministicSequence' "?"
exclaim = deterministicSequence' "!"
assert = deterministicSequence' "."

punctuation = uniformDist [question, exclaim, assert]

whatILike = let evenChance = eitherOr 0.5
            in like `andThen` (apple `evenChance` banana) `andThen` punctuation

main = do
  seq <- sample whatILike
  return $ V.toList seq
