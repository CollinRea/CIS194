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
evalStr s = pure eval <*> par s
  where par = parseExp Lit Add Mul


-- Exercise 3

-- New Expr Type class
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit a = Lit a
  add x y = Add x y
  mul x y = Mul x y

reify :: ExprT -> ExprT
reify = id


-- Exercise 4

-- Instances of Expr for other Types

instance Expr Integer where
  lit a = a
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit a
    | a > 0     = True
    | otherwise = False
  add x y = x || y
  mul x y = x && y