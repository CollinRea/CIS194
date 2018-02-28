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

-- foldTree :: [a] -> Tree a
-- foldTree = 

insert :: Eq a => a -> Tree a -> Tree a
insert v Leaf = Node 0 Leaf v Leaf
insert v t@(Node i left x right)
  | findT == left = Node newHeight (insert v left) x right
  | findT == right = Node newHeight left x (insert v right)
  where findT = compareHeight t
        newHeight = 
          if left == Leaf && right == Leaf then
            i
          else 
            i + 1 

compareHeight :: Tree a -> Tree a
compareHeight Leaf = Leaf
compareHeight (Node 0 left _ _) = left
compareHeight (Node _ l@(Leaf) _ _) = l
compareHeight (Node _ (Node _ _ _ _) _ r@(Leaf)) = r
compareHeight (Node _ l@(Node i _ _ _) _ r@(Node u _ _ _))
  | i <= u = l
  | otherwise = r 
