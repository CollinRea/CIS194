{-# OPTIONS_GHC -Wall #-}
module CodeGolf where


-- Exercise 1 Hopscotch

skips :: [a] -> [[a]]
skips [] = [[]]
skips [x] = [[x]]
skips l@(_:xs) = l : (skips xs)