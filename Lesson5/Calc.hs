{-# OPTIONS_GHC -Wall #-}
module Calc where

import ExprT
import Parser (parseExp)

-- Exercise 1

-- Basic Calculator evaluator function for ExprT Typeclass
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y


-- Exercise 2

-- Use Parser to take a String and eval a Maybe Int
-- This uses pure and applicative <*> to lift the calculation
-- up into the Functor space rather than pattern matching it out
-- And wrapping it back up 
evalStr :: String -> Maybe Integer
evalStr s = (pure eval) <*> par s
  where par = parseExp Lit Add Mul