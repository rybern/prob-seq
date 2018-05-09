{-# Language
        FlexibleContexts,
        UndecidableInstances,
        TypeSynonymInstances,
        DeriveGeneric,
        DeriveDataTypeable,
        StandaloneDeriving #-}
-- | Fix-point type. It allows to define generic recursion schemes.
--
-- > Fix f = f (Fix f)
--
-- Type @f@ should be a 'Functor' if you want to use
-- simple recursion schemes or 'Traversable' if you want to
-- use monadic recursion schemes. This style allows you to express
-- recursive functions in non-recursive manner.
-- You can imagine that a non-recursive function
-- holds values of the previous iteration.
--
-- Little example:
--
-- > type List a = Fix (L a)
-- >
-- > data L a b = Nil | Cons a b
-- >
-- > instance Functor (L a) where
-- >    fmap f x = case x of
-- >        Nil      -> Nil
-- >        Cons a b -> Cons a (f b)
-- >
-- > length :: List a -> Int
-- > length = cata $ \x -> case x of
-- >    Nil      -> 0
-- >    Cons _ n -> n + 1
-- >
-- > sum :: Num a => List a -> a
-- > sum = cata $ \x -> case x of
-- >    Nil      -> 0
-- >    Cons a s -> a + s

module Data.Fix (
    Fix(..)
    -- * Simple recursion
    -- | Type @f@ should be a 'Functor'. They transform
    -- non-recursive functions to recursive ones.
    , cata
    , ana
    , hylo
    , (~>)
    -- * Monadic recursion
    -- | Type @f@ should be a 'Traversable'.
    , cataM
    , anaM
    , hyloM
    )
where

import GHC.Generics
import Control.Applicative
import Data.Data
import Data.Function (on)
import Data.Traversable

-- | A fix-point type.
newtype Fix f = Fix { unFix :: f (Fix f) } deriving (Generic, Typeable)
deriving instance (Typeable f, Data (f (Fix f))) => Data (Fix f)

-- standard instances

instance Eq (f (Fix f)) => Eq (Fix f) where
    (==) = (==) `on` unFix

instance Ord (f (Fix f)) => Ord (Fix f) where
    compare = compare `on` unFix


-- recursion

-- | Catamorphism or generic function fold.
cata :: Functor f => (f a -> a) -> (Fix f -> a)
cata f = f . fmap (cata f) . unFix

-- | Anamorphism or generic function unfold.
ana :: Functor f => (a -> f a) -> (a -> Fix f)
ana f = Fix . fmap (ana f) . f

-- | Hylomorphism is anamorphism followed by catamorphism.
hylo :: Functor f => (f b -> b) -> (a -> f a) -> (a -> b)
hylo phi psi = cata phi . ana psi

-- | Infix version of @hylo@.
(~>) :: Functor f => (a -> f a) -> (f b -> b) -> (a -> b)
psi ~> phi = phi . fmap (hylo phi psi) . psi

-- monadic recursion

-- | Monadic catamorphism.
cataM :: (Applicative m, Monad m, Traversable t)
    => (t a -> m a) -> Fix t -> m a
cataM f = (f =<<) . traverse (cataM f) . unFix

-- | Monadic anamorphism.
anaM :: (Applicative m, Monad m, Traversable t)
    => (a -> m (t a)) -> (a -> m (Fix t))
anaM f = fmap Fix . (traverse (anaM f) =<<) . f

-- | Monadic hylomorphism.
hyloM :: (Applicative m, Monad m, Traversable t)
    => (t b -> m b) -> (a -> m (t a)) -> (a -> m b)
hyloM phi psi = (cataM phi =<<) . anaM psi
