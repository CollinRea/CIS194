{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)


-- Exercise 1 

-- Append function or JoinList
(+++) :: Monoid m => 
        JoinList m a  -> JoinList m a -> JoinList m a
l1 +++ l2 = Append (tag l1 `mappend` tag l2) l1 l2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m


-- Excercise 2

-- Get value at index of JoinList
indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ _ Empty          = Nothing
indexJ i _ | i < 0      = Nothing
indexJ 0 (Single _ v)   = Just v
indexJ _ (Single _ _ )  = Nothing
indexJ i (Append _ l1 l2) 
  | i < sizel || i == 0       = indexJ i l1
  | i >= sizel && sizel > 0   = indexJ (i - sizel) l2
  | otherwise                 = indexJ (i-1) l2
  where sizel = getSize $ size $ tag $ l1

-- Create List from JoinList
jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Find Maybe value at index with List
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:_) !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i-1)