{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee
import Data.Tree

-- Exercise 1

-- 1. Add Employee to GuestList
glCons :: Employee -> GuestList -> GuestList
glCons e (GL gl f) = GL (e : gl) (f + empFun e)

-- 2. Monoid of GuestList
instance Monoid GuestList where
  mempty  = (GL [] 0)
  mappend (GL gl1 f1) (GL gl2 f2) 
    = GL (gl1 ++ gl2) (f1 + f2)

-- 3. compare GuestLists
moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 
  | gl1 > gl2 = gl1
  | otherwise = gl2


-- Exercise 2

-- Rose Tree Fold function
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f = go where
    go (Node x ts) = f x (map go ts)


-- Exercise 3

-- Take boss employee and list of pairs of GuestLists
-- Combine guest lists (1st Pair is with bosses, 2nd pair is without)
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel b gls = foldr mappend (b `glCons` mempty,mempty) gls


-- Exercise 4

-- get Guests Lists (without , with boss)
guestLists :: Tree Employee -> (GuestList, GuestList)
guestLists (Node e []) = (mempty, GL [e] $ empFun e)
guestLists (Node e es) =
  (mconcat $ map (uncurry moreFun) $ withBoss es,
  GL [e] (empFun e) `mappend` (mconcat $ withoutBoss es))
  where withBoss es = map guestLists es
        withoutBoss es = map fst $ withBoss es


-- Find max Fun Guest List given a Tree of Employees
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . guestLists

