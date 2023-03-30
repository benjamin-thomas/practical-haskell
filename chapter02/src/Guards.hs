{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant guard" #-}

module Guards where

import Section02 (rev)

mFib1 :: Integer -> Maybe Integer
mFib1 n =
    if n < 0
        then Nothing
        else case n of
            0 -> Just 0
            1 -> Just 1
            n' ->
                let Just f1 = mFib1 (n' - 1)
                    Just f2 = mFib1 (n' - 2)
                 in Just (f1 + f2)

mFib2 :: Integer -> Maybe Integer
mFib2 n | n < 0 = Nothing
mFib2 0 = Just 0
mFib2 1 = Just 1
mFib2 n
    | otherwise =
        let Just f1 = mFib2 (n - 1)
            Just f2 = mFib2 (n - 2)
         in Just (f1 + f2)

{-
Binomial coefficient of `n` and `k`.

Suppose you have a bag containing 6 balls numbered 1 to 6.
You want to select 3 balls from the bag without regard to order.
How many different combinations of 3 balls can you choose from the bag?

To solve this problem, we can use the binomial coefficient formula as follows:

n choose k = n! / (k! * (n-k)!)

In this case, n = 6 (since there are 6 balls in the bag), and k = 3 (since we want to select 3 balls).

So we have:

    6 choose 3 = 6! / (3! * (6-3)!)
    = (6 * 5 * 4 * 3 * 2 * 1) / [(3 * 2 * 1) * (3 * 2 * 1)]
    = 20

Therefore, there are 20 different combinations of 3 balls that can be chosen from the bag, without regard to order.

These combinations are:

    {1, 2, 3}   {1, 2, 4}   {1, 2, 5}   {1, 2, 6}   {1, 3, 4}
    {1, 3, 5}   {1, 3, 6}   {1, 4, 5}   {1, 4, 6}   {1, 5, 6}
    {2, 3, 4}   {2, 3, 5}   {2, 3, 6}   {2, 4, 5}   {2, 4, 6}
    {2, 5, 6}   {3, 4, 5}   {3, 4, 6}   {3, 5, 6}   {4, 5, 6}

The formula given in the book recursively calculates the binomial coefficient of `n` and `k` differently, i.e.

    6 choose 3 = 5 choose 2 + 5 choose 3
    Guards.binom 6 3 == (Guards.binom 5 2) + (Guards.binom 5 3)
 -}
binom :: (Eq t, Num t, Num a) => t -> t -> a
binom _ 0 = 1
binom x y | x == y = 1
binom n k = binom (n - 1) (k - 1) + binom (n - 1) k

{-
    Any expression can be used in a guard
 -}

multipleOf :: Integer -> Integer -> Bool
multipleOf x y = mod x y == 0

specialMultiples :: Integer -> String
specialMultiples n | multipleOf n 2 = show n ++ " is multiple of 2"
specialMultiples n | multipleOf n 3 = show n ++ " is multiple of 3"
specialMultiples n | multipleOf n 5 = show n ++ " is multiple of 5"
specialMultiples n | otherwise = show n ++ " is a beautiful number"

specialMultiples2 :: Integer -> String
specialMultiples2 n
    | multipleOf n 2 = show n ++ " is multiple of 2"
    | multipleOf n 3 = show n ++ " is multiple of 3"
    | multipleOf n 5 = show n ++ " is multiple of 5"
    | otherwise = show n ++ " is a beautiful number"

-- EXERCISES 2-6

{-
    Define the famous Ackermann function. Try using guards.
 -}

{- FOURMOLU_DISABLE -}
ack :: Integer -> Integer -> Integer
ack m n
    | m == 0          = n + 1
    | m > 0 && n == 0 = ack (m - 1) 1
    | m > 0 && n > 0  = ack (m - 1) (ack m (n - 1))
    | otherwise       = error "Invalid args"
{- FOURMOLU_ENABLE -}

{-
    Define the `unzip` function, which takes a list of tuples and returns two lists,
    one with the first components and the other with the seconds.

    Here's an example:

    unzip [(1,2), (3,4)] = ([1,3], [2,4])
 -}

unzip :: [(a, b)] -> ([a], [b])
unzip lst =
    let
        inner lst' accA accB =
            case lst' of
                [] ->
                    (rev accA, rev accB)
                (a, b) : xs ->
                    inner xs (a : accA) (b : accB)
     in
        inner lst [] []