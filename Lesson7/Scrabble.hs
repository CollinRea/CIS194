{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}


module Scrabble where

import Data.Monoid
import Data.Char(toUpper)
-- import Sized
-- import JoinList

-- Exercise 3

-- Scrabble instance
newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

score :: Char -> Score
score c 
  | ch `elem` "AEILNORSTU" = Score 1
  | ch `elem` "DG"         = Score 2
  | ch `elem` "BCMP"       = Score 3
  | ch `elem` "FHVWY"      = Score 4
  | ch == 'K'              = Score 5
  | ch `elem` "JX"         = Score 8
  | ch `elem` "QZ"         = Score 10
  | otherwise              = Score 0
  where ch = toUpper c

scoreString :: String -> Score
scoreString []  = Score 0
scoreString (x:xs) = (score x) + scoreString xs