Partition.lhs

> module Partition where

> import Data.Maybe
> import Control.Monad
> import Data.List (nub)

Consider 3d isotropic harmonic oscilator.

> data State = State Int Int Int
>   deriving (Show, Eq)

> isProperState :: State -> Bool
> isProperState (State a b c) -- They should be non-negative.
>   = (a >= 0) && (b >= 0) && (c >= 0)
>
> energy :: State -> Int
> energy (State a b c) = a+b+c
>
> vacume :: State
> vacume = State 0 0 0
>
> creation, annihilation :: Int -> State -> Maybe State
> creation 1 (State a b c) = Just $ State (a+1) b     c
> creation 2 (State a b c) = Just $ State a     (b+1) c
> creation 3 (State a b c) = Just $ State a     b     (c+1)
> creation _ _             = Nothing
> 
> annihilation 1 (State a b c)
>   | a > 0     = Just $ State (a-1) b c
>   | otherwise = Nothing
> annihilation 2 (State a b c)
>   | b > 0     = Just $ State a (b-1) c
>   | otherwise = Nothing
> annihilation 3 (State a b c)
>   | c > 0     = Just $ State a b (c-1)
>   | otherwise = Nothing
> annihilation _ _ = Nothing

  *Partition> return vacume  >>= (creation 1)
  Just (State 1 0 0)
  *Partition> it >>= (creation 3) >>= (creation 3)
  Just (State 1 0 2)
  *Partition> it >>= (annihilation 1)
  Just (State 0 0 2)
  *Partition> it >>= (annihilation 1)
  Nothing

> nextStep (State a b c)
>   | a > 0     = [State a' (b+1) c, State a' b (c+1)]
>   | otherwise = []   
>   where a' = (a-1)

  *Partition> let s3 = State 3 0 0
  *Partition> s3
  State 3 0 0
  *Partition> nextStep s3
  [State 2 1 0,State 2 0 1]
  *Partition> nub . join . map nextStep $ it
  [State 1 2 0,State 1 1 1,State 1 0 2]
  *Partition> nub . join . map nextStep $ it
  [State 0 3 0,State 0 2 1,State 0 1 2,State 0 0 3]
  *Partition> nub . join . map nextStep $ it
  []

> nextStep' n (State a b c)
>   | a >= n = [ State (a-n) (b+n+i) (c-i)
>             | i <- [(-n) .. n]
>             , (b+n+i) >= 0, (c-i) >= 0
>             ]
>   | otherwise = []

  *Partition> map (\n -> nextStep' n (State 3 0 0)) [0..3]
  [[State 3 0 0]
  ,[State 2 0 1,State 2 1 0]
  ,[State 1 0 2,State 1 1 1,State 1 2 0]
  ,[State 0 0 3,State 0 1 2,State 0 2 1,State 0 3 0]
  ]
  *Partition> join it
  [State 3 0 0,State 2 0 1,State 2 1 0,State 1 0 2,State 1 1 1,State 1 2 0,State 0 0 3,State 0 1 2,State 0 2 1,State 0 3 0]
     
> partition' :: Int -> [[State]]
> partition' num = map (\n -> nextStep' n (State num 0 0)) [0..num]
>
> partition = join . partition'

  *Partition> partition' 3
  [[State 3 0 0]
  ,[State 2 0 1,State 2 1 0]
  ,[State 1 0 2,State 1 1 1,State 1 2 0]
  ,[State 0 0 3,State 0 1 2,State 0 2 1,State 0 3 0]
  ]
  *Partition> join it
  [State 3 0 0,State 2 0 1,State 2 1 0,State 1 0 2,State 1 1 1,State 1 2 0,State 0 0 3,State 0 1 2,State 0 2 1,State 0 3 0]
  *Partition> length it
  10

> degeneracy :: Int -> Int
> degeneracy n = (n+1)*(n+2) `div` 2
