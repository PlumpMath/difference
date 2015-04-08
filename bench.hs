{-# LANGUAGE BangPatterns #-}

import Criterion
import Criterion.Main

import Data.Semigroup.Difference as D
import Data.Foldable as F

import NE

import Prelude hiding (foldr)

bigList :: NE Int
bigList = foldr (:>) (One 0) [1..100000]

sumD, sumD0, sumF, sumF1, sumMy, sumMy0, sumMy', sumMy0' :: NE Int -> Int
sumD   = D.foldl1 (\ x y -> x + y)
sumD0  = D.foldl0 (\ x y -> x + y) 0
sumF   = F.foldl1 (\ x y -> x + y)
sumF1  = F.foldl  (\ x y -> x + y) 0
sumMy  = myfoldl1 (\ x y -> x + y)
sumMy0 = myfoldl0 (\ x y -> x + y) 0
sumMy'  = myfoldl1' (\ x y -> x + y)
sumMy0' = myfoldl0' (\ x y -> x + y) 0

main :: IO ()
main = defaultMain [
    bgroup "sumBigList" [
        bench "D.foldl1" $ whnf sumD   bigList,
        bench "D.foldl0" $ whnf sumD0  bigList,
        bench "F.foldl1" $ whnf sumF   bigList,
        bench "F.foldl"  $ whnf sumF1  bigList,
        bench "myfoldl1" $ whnf sumMy  bigList,
        bench "myfoldl0" $ whnf sumMy0 bigList,
        bench "myfoldl1'" $ whnf sumMy'  bigList,
        bench "myfoldl0'" $ whnf sumMy0' bigList
        ] ]
