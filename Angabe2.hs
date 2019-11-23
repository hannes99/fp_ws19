module Angabe2 where

import Data.List(sort)

type Nat1 = Integer

reversed :: Nat1 -> Nat1
reversed = read . reverse . show

-- prüft ob eine Zahl eine Primzahl ist
is_prime :: Nat1 -> Bool
is_prime 1 = False
is_prime 2 = True
is_prime n = (length [x | x <- [2 .. (div n 2)], mod n x == 0]) == 0

dp :: (Nat1,Nat1) -> [Nat1]
dp (a,b) = [ x | x <- [(min a b)..(max a b)], (is_prime x && (is_prime (reversed x)))]

folge_ohne_zyklus :: [Nat1] -> Nat1 -> [Nat1]
folge_ohne_zyklus seen n
    | elem n [1, sum_of_dividers] || elem sum_of_dividers seen = []
    | otherwise = sum_of_dividers : (folge_ohne_zyklus (n:seen) (sum_of_dividers))
  where
    sum_of_dividers :: Nat1
    sum_of_dividers = sum [ x | x <- [1..(div n 2)], mod n x == 0 ]

folge :: Nat1 -> [Nat1]
folge n = [n] ++ folge_ohne_zyklus [] n

sorted_contains_dups :: [Int] -> Bool
sorted_contains_dups [] = False
sorted_contains_dups (l:ls) = or (zipWith (==) (l : ls) (ls ++ [l]))

sorted_medianoid :: [Int] -> Int
sorted_medianoid [] = 0
sorted_medianoid l
    | sorted_contains_dups l = sum l
    | otherwise = l !! (div (length l) 2)

medianoid :: [Int] -> Int
medianoid = sorted_medianoid . sort
