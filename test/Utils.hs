{-# LANGUAGE InstanceSigs #-}
module Utils where

import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Test.QuickCheck.Property
import Test.QuickCheck
import Control.Monad.Random

mkGen :: (QCGen -> a) -> Gen a
mkGen f = MkGen $ \g _ -> f g

instance MonadRandom Gen where
    getRandom = mkGen (fst . random)
    getRandoms = mkGen randoms
    getRandomR range = mkGen (fst . randomR range)
    getRandomRs range = mkGen (randomRs range)
