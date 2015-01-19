{-# OPTIONS_GHC -Wall #-}

-- Exercise 1

-- Converts positive integers to a list of digits. For 0 or negative inputs,
-- returns the empty list.
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- Does the same thing as toDigits except returns the list in reverse.
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

-- Exercise 2

-- Doubles every other integer in a list, beginning from the right. This means
-- that the second-to-last, fourth-to-last, etc. numbers are doubled.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse (doubleEveryOther' (reverse x))

-- Helper function because it's easier to do in reverse.
doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' [x] = [x]
doubleEveryOther' (x : y : zs) = x : y * 2 : doubleEveryOther' zs

-- Exercise 3

-- Calculates the sum of all of the digits in the list of numbers.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs) = sum (toDigits x) + sumDigits xs

-- Exercise 4

-- Indicates whether an Integer could be a valid credit card number.
-- 1) Double the value of every second digit beginning from the right.
-- 2) Add the digits of the doubled values and the undoubled digits from the
--    original number.
-- 3) Calculate the remainder when the sum is divided by 10.
-- 4) If the result equals 0, then the number is valid.
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0
