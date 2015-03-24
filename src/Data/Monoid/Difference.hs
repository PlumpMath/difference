module Data.Monoid.Difference (

    Endo(..),

-- ** Difference monoid
    improve,
    unimprove,

) where

import Data.Monoid

-- | Converting a normal monoid to a difference monoid.
improve :: Monoid a => a -> Endo a
improve a = Endo (mappend a)
{-# INLINE improve #-}

-- | Converting a difference monoid back to a normal monoid.
unimprove :: Monoid a => Endo a -> a
unimprove (Endo f) = f mempty
{-# INLINE unimprove #-}
