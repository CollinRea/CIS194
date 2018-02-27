{-# OPTIONS_GHC -Wall #-}
module Lesson4 where


-- Exercise 1 Wholemeal Programming

-- Remake fun1 and fun2 functions with $ and . 
fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

-- Concise version
fun1' :: [Integer] ->  Integer
fun1' xs = foldr ((*) . subtract 2) 1 $ takeWhile even xs 


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer 
fun2' =  sum . filter even . takeWhile (>1) . iterate (\x -> if odd x then 3 * x + 1 else x `div` 2)