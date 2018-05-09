{-# LANGUAGE OverloadedLists #-}
import Test.Tasty

import TestConstructors

main :: IO ()
main = do
  defaultMain operationTests
