module Data.Monoid.Difference (

-- ** Difference monoid
    improve,
    unimprove,

) where

import Data.Monoid

-- | Converting a normal monoid to a difference monoid.
improve :: Monoid a => a -> Endo a
improve a = Endo (mappend a)

-- | Converting a difference monoid back to a normal monoid.
unimprove :: Monoid a => Endo a -> a
unimprove (Endo f) = f mempty
