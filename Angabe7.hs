module Angabe7 where

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
generiere_exp_strom x = [sum (take z (zipWith (/) [x**n | n <- [0..]] (1:1:[product [1..n] | n <- [2..]]))) | z <- [1..]]

selektiere :: Genauigkeit -> Strom -> Approx_Wert
selektiere g s = snd $ head (dropWhile ((<=) g . fst) (zip (zipWith (-) (tail s) s) s))
