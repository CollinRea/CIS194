{-# OPTIONS_GHC -Wall #-}

-- Lesson 1 : Credit Card Number validation


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
toDigitsRev x = 
    reverse (toDigits x)


-- Exercise 2

-- Take a list of Integers, and double every other one
doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' [x] = [x]
doubleEveryOther' (x:y:xs) = 
    [x] ++ [y * 2] ++ doubleEveryOther' xs 

-- Call doubleEveryOther' but in reverse
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs =
    reverse ( doubleEveryOther' (reverse xs))


-- Exercise 3

-- Get list of single digits
-- ex [16,7,12,5] = [1, 6, 7, 1, 2, 5]
-- notice Point Free style using function composition (.)
getDigits :: [Integer] -> [Integer]
getDigits =
    concat . map toDigits

-- Sum all the digits
-- using normal style with ($) to get rid of parens
sumDigits :: [Integer] -> Integer
sumDigits xs = 
    sum $ getDigits xs


-- Exercise 4

-- Compose functions using Point Free again
composeVal :: Integer -> Integer
composeVal =
  sumDigits . doubleEveryOther . toDigits

-- Validate Function
validate :: Integer -> Bool
validate x =
    mod (composeVal x) 10 == 0
