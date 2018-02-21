{-# OPTIONS_GHC -Wall #-}

-- Exercise 1

-- Use modulo to get remainder of X / 10 which gives last digit in list
-- Concat that digit in a list to the recursive call to get the remaining digits
toDigits :: Integer -> [Integer]
toDigits x 
    | x <= 0 = []
    | otherwise = toDigits (div x 10) ++ [mod x 10]

-- You can redo the function and put [mod x 10] before recursive call
-- Or just use built in list reverse function
toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

