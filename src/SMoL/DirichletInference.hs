{-# LANGUAGE BangPatterns, ViewPatterns, RankNTypes, FlexibleContexts #-}
module SMoL.DirichletInference where

import SMoL
import SMoL.Tags
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
-- import Numeric.AD
import Control.Applicative
import Numeric.AD.Mode.Reverse as Reverse
import Numeric.AD.Internal.Reverse (Tape)
import Math.Gamma
import Data.Reflection

type DirichletTag = ([Double], Tag Int)
type Queries = Reader [(Posterior, TagIxs)]

mapQueries :: Query a -> Queries [a]
mapQueries q = do
  vals <- ask
  return $ map (\val -> runReader q val) vals

toQuery :: Queries a -> Query a
toQuery q = do
  val <- ask
  return $ runReader q [val]

branchMAP :: DirichletTag -> Query [[Double]]
branchMAP tag = toQuery (branchMAPs tag)

branchMAPs :: DirichletTag -> Queries [[Double]]
branchMAPs (prior, tag) = do
  likelihoodMaps <- mapQueries (tagDist tag)
  let getLikelihood likelihoodMap =
        case mapM (flip Map.lookup likelihoodMap) [1..length prior] of
          (Just likelihoods) -> likelihoods
          Nothing -> case mapM (flip Map.lookup likelihoodMap) [0..length prior-1] of
            (Just likelihoods) -> likelihoods
            Nothing -> error (show likelihoodMap ++ ", " ++ show (length prior))
      likelihoodss = map getLikelihood likelihoodMaps
      steps = dirichletMAP prior likelihoodss
  return steps

takeUntilPair :: (a -> a -> Bool) -> [a] -> [a]
takeUntilPair p l@(y:x:xs) = if p y x then [y] else y:takeUntilPair p (x:xs)
takeUntilPair _ xs = xs

dropUntilPair :: (a -> a -> Bool) -> [a] -> [a]
dropUntilPair p l@(y:x:xs) = if p y x then l else dropUntilPair p (x:xs)
dropUntilPair _ xs = xs

converged :: (Ord a, Traversable t, Applicative t, Num a) => a -> t a -> t a -> Bool
converged eps xs1 xs2 = (< eps) . maximum $ liftA2 (\x1 x2 -> abs (x1 - x2)) xs1 xs2

test = dirichletMAP [3, 3, 3] (replicate 10 [0.2, 0.5, 0.3])
test' = dirichletGrad [0.1, 0.8, 0.1] [[0.1, 0.8, 0.1]]

normalizePs :: (Traversable t, Fractional a, Eq a, Ord a) => t a -> t a
normalizePs = fmap (max 0.0001 . min 1) . normalize

dirichletMAP :: [Double] -> [[Double]] -> [[Double]]
dirichletMAP psPrior branchLikelihoods = (initial:) $ gradientAscent posterior normalizePs initial
  where posterior :: (Floating a, Real a) => [a] -> a
        posterior = dirichletPosterior (map realToFrac psPrior) (map (map realToFrac) branchLikelihoods)
        initial = normalize psPrior

dirichletGrad :: [Double] -> [[Double]] -> [Double]
dirichletGrad psPrior branchLikelihoods = grad posterior psPrior
  where posterior :: (Floating a, Real a) => [a] -> a
        posterior = dirichletPosterior (map realToFrac psPrior) (map (map realToFrac) branchLikelihoods)

dirichletPosterior :: (Floating a, Real a) => [a] -> [[a]] -> [a] -> a
dirichletPosterior psPrior branchLikelihoods (normalize -> ps) =
  dirichletLogDensity psPrior ps + sum (map (\lls -> log (sum (zipWith (*) lls ps))) branchLikelihoods)

dirichletLogDensity :: (Floating a, Real a) => [a] -> [a] -> a
dirichletLogDensity alphas = \xs -> (logDenom +) . sum $ zipWith (\alpha x -> log x * (alpha - 1)) alphas xs
  where logDenom = log $ 1 / realToFrac (beta (map realToFrac alphas))

beta :: [Double] -> Double
beta alphas = product (map gamma alphas) / gamma (sum alphas)

gradientAscent :: (Traversable f, Fractional a, Ord a)
                => (forall s. Reifies s Tape => f (Reverse s a) -> Reverse s a)
                -> (f a -> f a)
                -> f a
                -> [f a]
gradientAscent f = gradientDescent (negate . f)

gradientDescent :: (Traversable f, Fractional a, Ord a)
                => (forall s. Reifies s Tape => f (Reverse s a) -> Reverse s a)
                -> (f a -> f a)
                -> f a
                -> [f a]
gradientDescent f fix x0 = go x0 fx0 xgx0 0.1 (0 :: Int)
  where
    (fx0, xgx0) = Reverse.gradWith' (,) f x0
    go x fx xgx !eta !i
      | eta == 0     = []
      | fx1 > fx     = go x fx xgx (eta/2) 0
      | zeroGrad xgx = []
      | otherwise    = x1 : if i == 10
                            then go x1 fx1 xgx1 (eta*2) 0
                            else go x1 fx1 xgx1 eta (i+1)
      where
        zeroGrad = all (\(_,g) -> g == 0)
        x1 = fix $ fmap (\(xi,gxi) -> xi - eta * gxi) xgx
        (fx1, xgx1) = Reverse.gradWith' (,) f x1
