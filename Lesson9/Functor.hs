{-# LANGUAGE InstanceSigs #-}

module Functor where

data Either' e a = Left' e | Right' a
  deriving (Eq, Ord, Read, Show)

instance Functor (Either' e) where
  fmap :: (a -> b) -> Either' e a -> Either' e b
  fmap _ (Left' e) = Left' e
  fmap g (Right' x) = Right' (g x)
  
