{-# OPTIONS_GHC -Wall #-}
module Lesson4 where

import Data.List ((\\))

-- Exercise 1 Wholemeal Programming

-- Remake fun1 and fun2 functions with $ and . 
fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

-- 'Wholemeal' version
fun1' :: [Integer] ->  Integer
fun1' xs = foldr ((*) . subtract 2) 1 $ takeWhile even xs 

-- Original version
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

-- 'Wholemeal' version
fun2' :: Integer -> Integer 
fun2' =  sum . filter even . takeWhile (>1) . iterate (\x -> if odd x then 3 * x + 1 else x `div` 2)


-- Exercise 2 Folding with trees

-- Notes
data Tree a 
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- Builds balanced tree using insert helper function
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- Takes a tree and compares the inner "Nodes" to see which 
-- side it should insert in and tries to increment the height
-- Not currently working properly
insert :: a -> Tree a -> Tree a
insert v Leaf = Node 0 Leaf v Leaf
insert v (Node i l@(Leaf) x right) = 
  Node (i + 1) (insert v l) x right
insert v (Node i left x r@(Leaf)) = 
  Node i left x (insert v r)
insert v (Node i l@(Node u _ _ _) x r@(Node y _ _ _))
  | u <= y = Node (i + 1) (insert v l) x r
  | otherwise = Node i l x (insert v r)


-- Exercise 3 More Folds!

-- Return True only if there are Odd number of Trues in list
xor :: [Bool] -> Bool
xor = foldr (\x b -> if x then not b else b) False

-- Map function implemented with Fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs-> f x : xs) []

-- Foldl implemented with foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)


-- Exercise 4 Finding primes

-- Find all Odd prime numbers
sieveSundaram ::Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x+1) $ [1..n] \\ sieveSundelete n

-- Generates list to be deleted
sieveSundelete :: Integer -> [Integer]
sieveSundelete n = filter (\x -> 2*x+2 < n) $ sieveCalc n

-- List comprehension to generate list of Ints in sieve formulate
sieveCalc :: Integer -> [Integer]
sieveCalc n = [x | x <- [i+j+2*i*j | (i,j) <-cartProd [1..n] [1..n], i<=j]]

-- Cartesian product helper function
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]


