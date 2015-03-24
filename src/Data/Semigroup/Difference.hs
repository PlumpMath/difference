module Data.Semigroup.Difference (

-- ** Difference semigroup
    Diff(..),
    improve, unimprove,
    diffMaybe, maybeDiff,

-- ** Folds
    foldr1m, foldr1,
    foldl1m, foldl1,
    toList1,

) where

import Data.List.NonEmpty
import Data.Semigroup
import Data.Semigroup.Foldable

import Prelude hiding (foldr1, foldl1)


-- | Difference semigroup.
data Diff a = Diff (a -> a) a

instance Semigroup (Diff a) where
    ~(Diff a' _) <> ~(Diff b' b) = Diff (a' . b') (a' b)

instance Monoid a => Monoid (Diff a) where
    mempty = Diff id mempty
    mappend = (<>)

-- | Converting a normal semigroup to a difference semigroup.
improve :: Semigroup a => a -> Diff a
improve a = Diff (a <>) a

-- | Converting a difference semigroup back to a normal semigroup.
unimprove :: Diff a -> a
unimprove ~(Diff _ a) = a


-- | One direction of the isomorphism between @'Diff' a@ and @'Maybe' a -> a@.
maybeDiff :: Diff a -> Maybe a -> a
maybeDiff (Diff a' a) = maybe a a'

-- | The other direction of the isomorphism between @'Diff' a@ and @'Maybe' a -> a@.
diffMaybe :: (Maybe a -> a) -> Diff a
diffMaybe f = Diff (f . Just) (f Nothing)


-- These folds should really be in Data.Semigroup.Foldable.

-- | Right-associative fold of a nonempty structure, with a base case.
foldr1m :: Foldable1 t => (a -> Maybe b -> b) -> t a -> b
foldr1m f = unimprove . foldMap1 (diffMaybe . f)

-- | Right-associative fold of a nonempty structure.
foldr1 :: Foldable1 t => (a -> a -> a) -> t a -> a
foldr1 f = unimprove . foldMap1 (\a -> Diff (f a) a)

-- | Left-associative fold of a nonempty structure, with a base case.
foldl1m :: Foldable1 t => (Maybe b -> a -> b) -> t a -> b
foldl1m f = unimprove . getDual . foldMap1 (Dual . diffMaybe . flip f)

-- | Left-associative fold of a nonempty structure.
foldl1 :: Foldable1 t => (a -> a -> a) -> t a -> a
foldl1 f = unimprove . getDual . foldMap1 (\a -> Dual $ Diff (flip f a) a)

-- | Nonempty list of elements of a nonempty structure.
toList1 :: Foldable1 t => t a -> NonEmpty a
toList1 = foldr1m (\a -> maybe (a :| []) (a <|))
