module NE where

import Data.Foldable
import Data.Semigroup
import Data.Semigroup.Foldable

data NE a = One a | a :> NE a
    deriving (Eq,Ord,Show)

instance Functor NE where
    fmap f (One a) = One (f a)
    fmap f (a :> as) = f a :> fmap f as

instance Foldable NE where
    foldMap f (One a) = f a
    foldMap f (a :> as) = f a `mappend` foldMap f as

instance Foldable1 NE where
    foldMap1 f (One a) = f a
    foldMap1 f (a :> as) = f a <> foldMap1 f as

myfoldl1 :: (a -> a -> a) -> NE a -> a
myfoldl1 f (One a) = a
myfoldl1 f (a :> as) = go a as where
    go a (One b) = f a b
    go a (a' :> as') = go (f a a') as'

myfoldl1' :: (a -> a -> a) -> NE a -> a
myfoldl1' f (One a) = a
myfoldl1' f (a :> as) = go a as where
    go a (One b) = f a b
    go a (a' :> as') = let z = f a a' in z `seq` go z as'

myfoldl0 :: (b -> a -> b) -> b -> NE a -> b
myfoldl0 f z = go z where
    go b (One a) = f z a
    go b (a :> as) = go (f b a) as

myfoldl0' :: (b -> a -> b) -> b -> NE a -> b
myfoldl0' f z = go z where
    go b (One a) = f z a
    go b (a :> as) = let z = f b a in z `seq` go z as
