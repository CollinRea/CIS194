{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee

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
