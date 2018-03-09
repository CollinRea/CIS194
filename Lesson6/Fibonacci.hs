{-# OPTIONS_GHC -Wall #-}

module Fibonacci where

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

-- Faster Fibonacci sequence starts with first 2 numbers, then uses next function
-- to add them and call next again recursively forever
-- The really cool part that you may notice is we pass the tail to next, and due
-- to lazy evaluation, it just keeps going without actually knowing the number until
-- it needs it.
fibs2 :: [Integer]
fibs2 = [0,1] ++ next fibs2
  where
    next (x : xs@(y:_)) = (x + y) : next xs


-- Exercise 3

-- Streams
