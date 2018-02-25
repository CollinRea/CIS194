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


-- Exercise 2 Local maxima

-- If less than 3 in list, return empty list
-- Need to be able to compare the middle element
-- localMaxima [2,9,5,6,1] == [9,6]
localMaxima :: [Integer] -> [Integer]
localMaxima xs | length xs < 3 = []
localMaxima l@(x:y:z:_)
  | x < y && y > z = y : next
  | otherwise = next
  where next = localMaxima (drop 1 l)


-- Exercise 3 Histogram

-- Histogram takes the first list of Integers and creates
-- a list of Tuples, with the Number and how many there are using 
-- elemIndices to count the numbers in the list
histogram :: [Integer] -> String
histogram [] = "==========\n0123456789\n"
histogram xs = concat (visualize list ++ [histogram []])
  where
    list = [(n, length (elemIndices n xs)) | n <- [0..9]]

-- This function takes the list of tuples and builds the list of rows
-- Uses helper function convertRow to draw the "*" and " "
-- Also uses decr local function to decrement the 2nd number in the tuple
-- as we draw out each row
visualize :: [(Integer, Int)] -> [String]
visualize [] = []
visualize xs
  | any (\(_,n)-> n > 0) xs = visualize (decr xs) ++ convertRow xs
  | otherwise = []
  where
    decr = map (\(x,n)->(x,n-1))

-- Takes a list of Tuples and converts to list of strings
convertRow :: [(Integer, Int)] -> [String]
convertRow l = (map (\(_,n)-> if n > 0 then "*" else " ") l) ++ ["\n"]


-----------------------------------------------------------------
-- Didn't use this function but keeping it for reference

-- Replace String at given index in list and return new list
insertAt :: Int -> String-> [String] -> [String] 
insertAt z y xs = as ++ (y : bs)
  where (as,(_:bs)) = splitAt z xs
