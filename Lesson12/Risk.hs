{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }


-- Exercise 1

instance Show Battlefield where
  show (Battlefield a d) = show a ++ " vs " ++ show d

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield { attackers = numAtk, defenders = numDef }) =
  replicateM (min 3 $ numAtk - 1) die >>= \atks ->
  replicateM (min 2 numDef) die >>= \defs ->
  let (lossAtk, lossDef) = fight atks defs
  in return $ Battlefield (numAtk - lossAtk) (numDef - lossDef)

fight :: [DieValue] -> [DieValue] -> (Army, Army)
fight atks defs = let descAtks = sortBy (flip compare) $ unDV <$> atks
                    descDefs = sortBy (flip compare) $ unDV <$> defs
                    countLoss (a, d) (lossAtk, lossDef)
                      | a > d     = (lossAtk, lossDef + 1)
                      | otherwise = (lossAtk + 1, lossDef)
                in foldr countLoss (0, 0) $ zip descAtks descDefs


-- Exercise 2

invade :: Battlefield -> Rand StdGen Battlefield
invade b
  | defenders b == 0 || attackers b < 2 = return b
  | otherwise                           = battle b >>= invade


-- Exercise 3

successProb :: Battlefield -> Rand StdGen Double
successProb b =
  replicateM 1000 (invade b) >>= \bs ->
  let wins = length $ filter (\b' -> defenders b' == 0) bs
in return $ fromIntegral wins / 1000