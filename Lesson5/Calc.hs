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
  lit a = a > 0
  add x y = x || y
  mul x y = x && y


newtype MinMax  = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit a = MinMax a
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7  = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit a = Mod7 (mod a 7)
  add (Mod7 x) (Mod7 y) = Mod7 (mod (x + y) 7)
  mul (Mod7 x) (Mod7 y) = Mod7 (mod (x * y) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7