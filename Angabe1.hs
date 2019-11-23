module Angabe1 where

import Data.Bits((.&.))

-- Aufgabe 1
streiche :: String -> Int -> Char -> String
streiche s n c
  | n <= 0 = s
  | otherwise = remove_ith_c s n c 1
  where
    remove_ith_c :: String -> Int -> Char -> Int -> String
    remove_ith_c [] _ _ _ = []
    remove_ith_c (s:ss) n c counter
        | s == c && mod counter n == 0 = remove_ith_c ss n c (counter+1)
        | s == c = [s] ++ remove_ith_c ss n c (counter+1)
        | otherwise = [s] ++ remove_ith_c ss n c counter

-- Aufgabe 2
type Nat0 = Int
ist_umgekehrt_2er_potenz :: Nat0 -> Bool
ist_umgekehrt_2er_potenz 0 = False
ist_umgekehrt_2er_potenz 1 = True
ist_umgekehrt_2er_potenz n = is2pot (read (reverse (show n)))
  where
    is2pot :: Nat0 -> Bool
    is2pot n = n .&. (n-1) == 0

-- Aufgabe 3
groesstes_palindrom_in :: [Nat0] -> Int
groesstes_palindrom_in l = find_max_pali l (-1)
  where
    is_pali x = (reverse (show x) == show x)
    find_max_pali :: [Nat0] -> Int -> Int
    find_max_pali [] max_num = max_num
    find_max_pali (n:ns) max_num
        | is_pali n = find_max_pali ns (max n max_num)
        | otherwise = find_max_pali ns max_num
