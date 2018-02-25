{-# OPTIONS_GHC -Wall #-}
module CodeGolf where

import Data.List

-- Exercise 1 Hopscotch

-- Take a list, and return a list of lists
-- taking every n + 1 starting at 1 until the length of the list.
-- ie:  skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
skips :: [a] -> [[a]]
skips xs = [every n xs | n <- [1..len]]
  where len = length xs

-- Helper function to use in skips, takes a number and a list
-- returns a new list of every N in the list
every :: Int -> [a] -> [a]
every n xs = 
  case drop (n-1) xs of
    [] -> []
    (y:ys) -> y : every n ys
