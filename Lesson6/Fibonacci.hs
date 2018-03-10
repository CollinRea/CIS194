{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

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
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show (Cons x rest) = "(" ++ (show x) ++ "," ++ show rest ++ ")"

streamToList :: Stream a -> [a]
streamToList (Cons x rest) = x : streamToList rest


-- Exercise 4

-- Stream Functions
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x rest) = Cons (f x) (streamMap f rest)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed uf s = Cons s (streamFromSeed uf (uf s))


-- Exercise 5

-- Create Streams
nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap rulerFunc nats 
  where rulerFunc n = divCounter n 1

divCounter :: Integer -> Integer -> Integer
divCounter num pow 
  | num `mod` (2^pow) == 0 = 1 + divCounter num (pow + 1) 
  | otherwise              = 0


-- Exercise 6 (Extra Credit)

-- Fibonacci Numbers with Streams
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = (Cons n (streamRepeat 0))
  negate s = streamMap (negate) s
  (+) (Cons n1 r1) (Cons n2 r2) = Cons (n1+n2) ((+) r1 r2)
  (*) (Cons n1 r1) s2@(Cons n2 r2) = Cons (n1*n2) (streamMap (n1*) r2) + (r1*s2)
