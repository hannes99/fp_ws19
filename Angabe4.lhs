> module Angabe4 where
> import Numeric (showHex, showIntAtBase)
> import Data.Char(toUpper, intToDigit, digitToInt)
> import Data.List (foldl')
>
> toDec :: String -> Zett
> toDec = foldl' (\acc x -> acc * 2 + (toInteger (digitToInt x))) 0
>
> class Binaer a where
>   a_zu_binaer :: a -> String
>   binaer_zu_a :: String -> a
>
> data IN_1 = Eins | Nf IN_1
> data ZZ = Null | Plus IN_1 | Minus IN_1
> type Zett = Integer
>
> instance Binaer ZZ where
>   a_zu_binaer a@(Minus x) = "-" ++ a_zu_binaer (zznegate a)
>   a_zu_binaer x = showIntAtBase 2 intToDigit (von_ZZ_nach_Zett x) ""
>   binaer_zu_a (x:xs)
>        | x == '-' && toDec (xs) == 0 = Null
>        | x == '-' = Minus (z_nach_zz (toDec xs))
>        | toDec (x:xs) == 0 = Null
>        | otherwise = Plus (z_nach_zz (toDec (x:xs)))
>
> instance Binaer IN_1 where
>   a_zu_binaer x = showIntAtBase 2 intToDigit (zz_nach_z x 0) ""
>   binaer_zu_a x = z_nach_zz (toDec x)
>
> instance Binaer Integer where
>   a_zu_binaer x = showIntAtBase 2 intToDigit x ""
>   binaer_zu_a (x:xs)
>        | x == '-' = (-1) * toDec (xs)
>        | otherwise = toDec (x:xs)
>
> instance Eq ZZ where
>   (==) = gleich
>
> instance Ord ZZ where
>   (<=) = kgleich
>
> instance Enum ZZ where
>   toEnum = von_Zett_nach_ZZ . fromIntegral
>   fromEnum = fromIntegral . von_ZZ_nach_Zett
>
> instance Num ZZ where
>   (+) = plus
>   (*) = mal
>   abs (Minus n) = (Plus n)
>   abs n = n
>   signum (Minus n) = Minus Eins
>   signum (Plus n) = Plus Eins
>   fromInteger = von_Zett_nach_ZZ
>   negate = zznegate
>
> instance Show ZZ where
>   show Null = "0"
>   show a@(Minus x) = map toUpper ("-" ++ showHex (abs (von_ZZ_nach_Zett a)) "")
>   show a@(Plus x) = map toUpper (showHex (von_ZZ_nach_Zett a) "")
>
> instance Eq IN_1 where
>   (==) = gleichN
>
> von_Zett_nach_ZZ :: Zett -> ZZ
> von_Zett_nach_ZZ n
>   | n == 0 = Null
>   | n < 0 = Minus (z_nach_zz (n*(-1)))
>   | n > 0 = Plus (z_nach_zz n)
>
> von_ZZ_nach_Zett :: ZZ -> Zett
> von_ZZ_nach_Zett Null = 0
> von_ZZ_nach_Zett (Minus n) = zz_nach_z n (-1)
> von_ZZ_nach_Zett (Plus n) = zz_nach_z n 1
>
> zz_nach_z :: IN_1 -> Zett -> Zett
> zz_nach_z (Nf n) x = zz_nach_z n x + x
> zz_nach_z Eins x = x
>
> z_nach_zz :: Zett -> IN_1
> z_nach_zz n
>   | n == 1 = Eins
>   | otherwise = Nf (z_nach_zz (n-1))
>

-----------------------------------
Hilfsfunktionen zum rechnen in IN_1

>
> groesserN :: IN_1 -> IN_1 -> Bool
> groesserN (Nf n) (Nf m) = groesserN n m
> groesserN Eins Eins = False
> groesserN Eins _ = False
> groesserN _ Eins = True
>
> kleinerN :: IN_1 -> IN_1 -> Bool
> kleinerN (Nf n) (Nf m) = kleinerN n m
> kleinerN Eins Eins = False
> kleinerN Eins _ = True
> kleinerN _ Eins = False
>
> gleichN :: IN_1 -> IN_1 -> Bool
> gleichN n m = not (kleinerN n m) && not (groesserN n m)
>
> ungleichN :: IN_1 -> IN_1 -> Bool
> ungleichN n m = not (gleichN n m)
>
> kgleichN :: IN_1 -> IN_1 -> Bool
> kgleichN n m = kleinerN n m || gleichN n m
>
> ggleichN :: IN_1 -> IN_1 -> Bool
> ggleichN n m = groesserN n m || gleichN n m
>
> minusN :: IN_1 -> IN_1 -> IN_1
> minusN (Nf n) (Nf m) = minusN n m
> minusN Eins (Nf m) = m
> minusN (Nf n) Eins = n
>
> plusN :: IN_1 -> IN_1 -> IN_1
> plusN (Nf n) (Nf m) = Nf (Nf (plusN n m))
> plusN Eins m = Nf m
> plusN n Eins = Nf n
>
> malN :: IN_1 -> IN_1 -> IN_1
> malN n (Nf m) = plusN n (malN n m)
> malN Eins m = m
> malN n Eins = n
>
> durchN :: IN_1 -> IN_1 -> IN_1
> durchN n Eins = n
> durchN n m
>   | gleichN n m = Eins
>   | groesserN m (minusN n m) = Eins
>   | otherwise = Nf (durchN (minusN n m) m)
>
> zznegate :: ZZ -> ZZ
> zznegate (Plus n) = Minus n
> zznegate (Minus n) = Plus n
>

----------------------------------------
Implementierung der eigentlichen Funktionen

>
> kleiner :: ZZ -> ZZ -> Bool
> kleiner (Plus n) (Minus m) = False
> kleiner (Minus n) (Plus m) = True
> kleiner (Plus n) (Plus m) = kleinerN n m
> kleiner (Minus n) (Minus m) = kleinerN m n
> kleiner Null (Plus m) = True
> kleiner Null (Minus m) = False
> kleiner Null Null = False
> kleiner (Plus n) Null = False
> kleiner (Minus n) Null = True
>
> groesser :: ZZ -> ZZ -> Bool
> groesser (Plus n) (Minus m) = True
> groesser (Minus n) (Plus m) = False
> groesser (Plus n) (Plus m) = groesserN n m
> groesser (Minus n) (Minus m) = groesserN m n
> groesser Null (Minus m) = True
> groesser Null (Plus m) = False
> groesser Null Null = False
> groesser (Plus n) Null = True
> groesser (Minus n) Null = False
>
> gleich :: ZZ -> ZZ -> Bool
> gleich (Plus n) (Minus m) = False
> gleich (Minus n) (Plus m) = False
> gleich (Plus n) (Plus m) = gleichN n m
> gleich (Minus n) (Minus m) = gleichN n m
> gleich Null Null = True
> gleich Null _ = False
> gleich _ Null = False
>
> ungleich :: ZZ -> ZZ -> Bool
> ungleich (Plus n) (Minus m) = True
> ungleich (Minus n) (Plus m) = True
> ungleich (Plus n) (Plus m) = ungleichN n m
> ungleich (Minus n) (Minus m) = ungleichN n m
> ungleich Null Null = False
> ungleich Null _ = True
> ungleich _ Null = True
>
> kgleich :: ZZ -> ZZ -> Bool
> kgleich n m = gleich n m || kleiner n m
>
> ggleich :: ZZ -> ZZ -> Bool
> ggleich n m = gleich n m || groesser n m
>
> plus :: ZZ -> ZZ -> ZZ
> plus (Plus n) (Plus m) = Plus (plusN n m)
> plus (Plus n) (Minus m) = minus (Plus n) (Plus m)
> plus (Minus n) (Plus m)
>   | groesserN m n = Plus (minusN m n)
>   | kleinerN m n = Minus (minusN n m)
>   | otherwise = Null
> plus (Minus n) (Minus m) = Minus (plusN n m)
> plus Null m = m
> plus n Null = n
>
> minus :: ZZ -> ZZ -> ZZ
> minus (Plus n) (Plus m)
>   | groesserN m n = Minus (minusN n m)
>   | kleinerN m n = Plus (minusN n m)
>   | otherwise = Null
> minus (Plus n) (Minus m) = plus (Plus n) (Plus m)
> minus (Minus n) (Plus m) = Minus (plusN n m)
> minus (Minus n) (Minus m) = plus (Minus n) (Plus m)
> minus n Null = n
> minus Null m = zznegate m
>
> mal :: ZZ -> ZZ -> ZZ
> mal (Plus n) (Plus m) = Plus (malN n m)
> mal (Plus n) (Minus m) = Minus (malN n m)
> mal (Minus n) (Plus m) = Minus (malN n m)
> mal (Minus n) (Minus m) = Plus (malN n m)
> mal Null _ = Null
> mal _ Null = Null
>
> durch :: ZZ -> ZZ -> ZZ
> durch (Plus n) (Plus m)
>   | groesserN m n = Null
>   | otherwise = Plus (durchN n m)
> durch (Plus n) (Minus m)
>   | groesserN m n = Null
>   | otherwise = Minus (durchN n m)
> durch (Minus n) (Plus m)
>   | groesserN m n = Minus Eins
>   | gleich (mal (Minus div_in_n) (Minus m)) (Plus n) = Minus div_in_n
>   | otherwise = minus (Minus div_in_n) (Plus Eins)
>   where
>       div_in_n = durchN n m
> durch (Minus n) (Minus m)
>   | groesserN m n = Plus Eins
>   | gleich (mal (Plus div_in_n) (Minus m)) (Minus n) = Plus div_in_n
>   | otherwise = plus (Plus div_in_n) (Plus Eins)
>   where
>       div_in_n = durchN n m
> durch _ Null = Null
> durch Null _ = Null
