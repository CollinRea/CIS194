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

