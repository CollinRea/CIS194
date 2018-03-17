{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Sized
import Scrabble
import Buffer

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

-- 1. Get value at index of JoinList 
indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ v)   = Just v
indexJ _ Empty          = Nothing
indexJ i _ | i < 0      = Nothing
indexJ _ (Single _ _ )  = Nothing
indexJ i (Append _ l1 l2) 
  | i < sizel = indexJ i l1
  | otherwise = indexJ (i - sizel) l2
  where sizel = getSize $ size $ tag l1

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

-- 2. Drop first n of JoinList
dropJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> JoinList b a
dropJ _ Empty        = Empty
dropJ n jl | n <= 0  = jl
dropJ _ (Single _ _) = Empty
dropJ n (Append _ l1 l2)
  | n == sizel          = l2
  | n > sizel           = dropJ (n - sizel) l2
  | otherwise           = (dropJ n l1) +++ l2
  where sizel = getSize $ size $ tag l1
  
-- 3. Take first n of JoinList
takeJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> JoinList b a
takeJ _ Empty        = Empty
takeJ n _ | n <= 0   = Empty
takeJ n jl | n >= s  = jl
  where s = getSize $ size $ tag jl
takeJ n (Append _ l1 l2)
  | n == sizel  = l1
  | n > sizel   = l1 +++ (takeJ (n-sizel) l2)
  | otherwise   = takeJ n l1
  where sizel = getSize $ size $ tag l1


-- Exercise 3

-- Scrabble Scoreline
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s


-- Exercise 4

-- Pair of Monoids

instance Buffer (JoinList (Score, Size) String ) where
  toString = jlBuffToString

jlBuffToString :: JoinList (Score, Size) String -> String
jlBuffToString Empty = ""
jlBuffToString (Single (_,_) s) = s
jlBuffToString (Append (_,_) s1 s2) = 
  (jlBuffToString s1) ++ "\n" ++ (jlBuffToString s2) ++ "\n"
                  


-- instance Buffer String where
--   toString     = id
--   fromString   = id
--   line n b     = safeIndex n (lines b)
--   replaceLine n l b = unlines . uncurry replaceLine' . splitAt n . lines $ b
--       where replaceLine' pre [] = pre
--             replaceLine' pre (_:ls) = pre ++ l:ls
--   numLines     = length . lines
--   value        = length . words

-- class Buffer b where

--   -- | Convert a buffer to a String.
--   toString :: b -> String

--   -- | Create a buffer from a String.
--   fromString :: String -> b

--   -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
--   -- for out-of-bounds indices.
--   line :: Int -> b -> Maybe String

--   -- | @replaceLine n ln buf@ returns a modified version of @buf@,
--   --   with the @n@th line replaced by @ln@.  If the index is
--   --   out-of-bounds, the buffer should be returned unmodified.
--   replaceLine :: Int -> String -> b -> b

--   -- | Compute the number of lines in the buffer.
--   numLines :: b -> Int

--   -- | Compute the value of the buffer, i.e. the amount someone would
--   --   be paid for publishing the contents of the buffer.
--   value :: b -> Int
