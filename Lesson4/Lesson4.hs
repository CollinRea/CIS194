{-# OPTIONS_GHC -Wall #-}
module Lesson4 where


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
