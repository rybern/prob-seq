{-# LANGUAGE OverloadedLists #-}
module TestOperations where

import Data.Word
import Data.Monoid
import Data.List (find)
import Control.Monad
import qualified Data.Vector as V

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import Test.Tasty.HUnit

import Sequence
import Sequence.AST
import Sequence.Matrix.Operations
import Sequence.Matrix.Sampling
import Sequence.Matrix.Emissions
import ArbitraryAST

exact :: IO ()
exact = do
  (SmallProbSeq (ast, seq)) <- generate (arbitrary :: Gen (SmallProbSeq Word8))

  let tree = probTree ast
      astP = sequenceProbAST seq ast
      matP = stateSequenceProbability seq (buildMatSeq ast)

  print ast
  print tree
  print seq
  print $ astP
  print $ matP
