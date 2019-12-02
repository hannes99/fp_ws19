module Angabe7 where

import Data.List(sort)
import           Debug.Trace

-- A1
generiere_fak_strom :: [Integer]
generiere_fak_strom = 1:1:[product [1..n] | n <- [2..]]

-- A2
type IR_plus = Double -- Nur Werte echt groesser als null
type Stelle = Double
type Genauigkeit = IR_plus
type Approx_Wert = Double
type Strom = [Double]

approximiere_exp :: Stelle -> Genauigkeit -> Approx_Wert
approximiere_exp x epsilon = selektiere epsilon (generiere_exp_strom x)

-- zip the fak-stream and the x^n-stream with division until the z-th zelment and sum those up (z goes from 1 to inf.)
generiere_exp_strom :: Stelle -> Strom
generiere_exp_strom x = [sum (take z (zipWith (/) [x**n | n <- [0..]] (1:1:[product [1..n] | n <- [1..]]))) | z <- [1..]]

--
selektiere :: Genauigkeit -> Strom -> Approx_Wert
selektiere g s = (snd . head) (filter ((>=) g . fst) (zip (zipWith (-) s (0:s)) s))

-- A3
type Woerterstrom = [String]

generiere_woerter :: Woerterstrom
generiere_woerter = go [""]
    where
        go :: [String] -> [String]
        go woerter =
            let neue = concatMap (\w -> [w++"a", w++"b", w++"c"]) woerter
            in neue ++ go neue

filtere_palindrome :: Woerterstrom -> Woerterstrom
filtere_palindrome = filter (\x -> x == reverse x)

-- A4
type Wort = String
type Woerterbuch = [Wort] -- nur Werte endliche Laenge; keine Stroeme.
type Wortleiter = [Wort]

woerterbuch = ["awake","awaken","cat","dig","dog","fig","fin","fine","fog","log","rake","wake","wine"]

ist_aufsteigende_leiterstufe :: Wort -> Wort -> Bool
ist_aufsteigende_leiterstufe a@(x:xs) b@(y:ys)
    | a == b = False
    | x == y = ist_aufsteigende_leiterstufe xs ys
    | x /= y && length xs > length ys = xs == (y:ys) && y > x
    | x /= y && length xs < length ys = (x:xs) == ys && y > x
    | otherwise = xs == ys && y >= x
ist_aufsteigende_leiterstufe [] [] = True
ist_aufsteigende_leiterstufe [] [_] = True
ist_aufsteigende_leiterstufe _ _ = False


ist_aufsteigende_wortleiter :: [Wort] -> Bool
ist_aufsteigende_wortleiter [a,b] = ist_aufsteigende_leiterstufe a b
ist_aufsteigende_wortleiter (a:b:xs) = ist_aufsteigende_leiterstufe a b && ist_aufsteigende_wortleiter (b:xs)

gib_max_aufsteigende_wortleiter :: Woerterbuch -> Wortleiter
gib_max_aufsteigende_wortleiter b = helper (head sorted) sorted
    where
        sorted = sort b

longestSequence p = maximum . scanl (\count x -> if (p x) then count + 1 else 0) 0

helper :: Wort -> Woerterbuch -> Wortleiter
helper s [x]
    | ist_aufsteigende_leiterstufe s x = [s,x]
    | otherwise = []
helper s (w:ws)
    | trace ("helper with (keep: "++show (length keep_s)++"; n_keep: "++show (length discard_s)++")" ++ show s ++ " " ++ show w++" " ++ show ws) False = undefined
    | length keep_s > length discard_s = s:(helper w ws)
    | otherwise = helper w ws
  where
    keep_s = helper s ws
    discard_s = helper w ws

gmaw = gib_max_aufsteigende_wortleiter . sort
