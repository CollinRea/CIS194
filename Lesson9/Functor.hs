{-# LANGUAGE InstanceSigs #-}

module Functor where

import GHC.Base (ignore Data.Tuple)

data Either' e a = Left' e | Right' a
  deriving (Eq, Ord, Read, Show)

instance Functor (Either' e) where
  fmap :: (a -> b) -> Either' e a -> Either' e b
  fmap _ (Left' e) = Left' e
  fmap g (Right' x) = Right' (g x)
 
-- Instance of Functor for a Tuple
--instance Functor ((,) e) where
--  fmap :: (a -> b) -> (e,a) -> (e,b)
--  fmap g (e,x) = (e,(g x))
