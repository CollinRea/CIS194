{-# OPTIONS_GHC -Wall #-}

module Fibonacci where

import Data.List

-- Exercise 1

-- Fibonacci number for given integer
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Infinite list of all Fib numbers
fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2

-- Faster Fibonacci sequence
fibs2 :: [Integer]
fibs2 = foldl' (\x y -> (x ++ [fib y])) [] (take 20 [0..])
