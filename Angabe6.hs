module Angabe6 where

-- halolololodfgdh
import           Debug.Trace

data Arith_Variable = A1 | A2 | A3 | A4 | A5 | A6 deriving (Eq,Show,Enum)
data Log_Variable = L1 | L2 | L3 | L4 | L5 | L6 deriving (Eq,Show,Enum)
data Arith_Ausdruck = AK Int -- Arithmetische Konstante
    | AV Arith_Variable -- Arithmetische Variable
    | Plus Arith_Ausdruck Arith_Ausdruck -- Addition
    | Minus Arith_Ausdruck Arith_Ausdruck -- Subtraktion
    | Mal Arith_Ausdruck Arith_Ausdruck -- Multiplikation
    deriving (Eq,Show)
data Log_Ausdruck = LK Bool -- Logische Konstante
    | LV Log_Variable -- Logische Variable
    | Nicht Log_Ausdruck -- Logische Negation
    | Und Log_Ausdruck Log_Ausdruck -- Logische Konjunktion
    | Oder Log_Ausdruck Log_Ausdruck -- Logische Disjunktion
    | Gleich Arith_Ausdruck Arith_Ausdruck -- Wertgleichheit
    -- arith. Ausdruecke
    | Kleiner Arith_Ausdruck Arith_Ausdruck -- Linker Ausdruck
    -- echt wertkleiner
    -- als
    -- Ausdruck
    deriving (Eq,Show)
type Arith_Variablenbelegung = Arith_Variable -> Int -- Total definierte Abb.
type Log_Variablenbelegung = Log_Variable -> Bool -- Total definierte Abb.
type Variablenbelegung = (Arith_Variablenbelegung, Log_Variablenbelegung)
links :: (Either a b) -> a
links (Left x) = x
rechts :: (Either a b) -> b
rechts (Right y) = y

class Evaluierbar a where
    evaluiere :: a -> Variablenbelegung -> Either Int Bool

type Adresse = Int
type Sprungadresse = Adresse
data Anweisung =
    AZ Arith_Variable Arith_Ausdruck -- Wertzuweisung an
    -- arithmetische Variable
    | LZ Log_Variable Log_Ausdruck -- Wertzuweisung an
    -- logische Variable
    | FU Log_Ausdruck Sprungadresse Sprungadresse -- Fallunter-
    -- scheidung
    | BS Log_Ausdruck Sprungadresse -- Bedingter Sprung
    | US Sprungadresse -- Unbedingter Sprung
    | MP Adresse Anweisung -- Selbstmodifikation des
    -- Programms
    deriving Show
type Zustand = Variablenbelegung
type Anfangszustand = Zustand
type Endzustand = Zustand
type Zwischenzustand = Zustand
type Programm = [Anweisung]
type EPS = Programm

instance Evaluierbar Arith_Ausdruck where
    evaluiere (AK a) (av, lv) = Left a
    evaluiere (AV a) (av, lv) = Left (av a)
    evaluiere (Plus a b) (av, lv) =
        Left (links (evaluiere a (av, lv)) + links (evaluiere b (av, lv)))
    evaluiere (Minus a b) (av, lv) =
        Left (links (evaluiere a (av, lv)) - links (evaluiere b (av, lv)))
    evaluiere (Mal a b) (av, lv) =
        Left (links (evaluiere a (av, lv)) * links (evaluiere b (av, lv)))

instance Evaluierbar Log_Ausdruck where
    evaluiere (LK    a) (av, lv) = Right a
    evaluiere (LV    a) (av, lv) = Right (lv a)
    evaluiere (Nicht a) (av, lv) = Right (not (rechts (evaluiere a (av, lv))))
    evaluiere (Und a b) (av, lv) =
        Right (rechts (evaluiere a (av, lv)) && rechts (evaluiere b (av, lv)))
    evaluiere (Oder a b) (av, lv) =
        Right (rechts (evaluiere a (av, lv)) || rechts (evaluiere b (av, lv)))
    evaluiere (Gleich a b) (av, lv) =
        Right (links (evaluiere a (av, lv)) == links (evaluiere b (av, lv)))
    evaluiere (Kleiner a b) (av, lv) =
        Right (links (evaluiere a (av, lv)) < links (evaluiere b (av, lv)))

-- A1
interpretiere_1 :: EPS -> Anfangszustand -> Endzustand
interpretiere_1 prog az = inter_1 prog 0 az

modifyEPS
    :: ([Anweisung], [Anweisung])
    -> Anweisung
    -> Adresse
    -> Adresse
    -> (EPS, Adresse)
modifyEPS prog@(before, (x : after)) anw pos adr
    | adr < 0 || adr > length oldProg = (oldProg, pos + 1)
    | adr == length oldProg           = (oldProg ++ [anw], adr)
    | otherwise                       = (before ++ (anw : after), adr)
    where oldProg = uncurry (++) prog


exec_anw1 :: ([Anweisung], [Anweisung]) -> Int -> Zustand -> Endzustand
-- exec_anw1 prog@(before, ca : after) pos zst@(av, lv)
--     | trace (" " ++ show ca) False = undefined
exec_anw1 prog@(_, ((AZ var val) : _)) pos zst@(av, lv) = inter_1
    (uncurry (++) prog)
    (pos + 1)
    (\x -> if x == var then links (evaluiere val zst) else av x, lv)
exec_anw1 prog@(_, ((LZ var val) : _)) pos zst@(av, lv) = inter_1
    (uncurry (++) prog)
    (pos + 1)
    (av, \x -> if x == var then rechts (evaluiere val zst) else lv x)
exec_anw1 prog@(_, ((FU val adrOK adrNOK) : _)) _ zst@(av, lv) = inter_1
    (uncurry (++) prog)
    (if rechts (evaluiere val zst) then adrOK else adrNOK)
    (av, lv)
exec_anw1 prog@(_, ((BS val adrOK) : _)) pos zst@(av, lv) = inter_1
    (uncurry (++) prog)
    (if rechts (evaluiere val zst) then adrOK else pos + 1)
    (av, lv)
exec_anw1 prog@(_, ((US adr) : _)) pos zst@(av, lv) =
    inter_1 (uncurry (++) prog) adr (av, lv)
exec_anw1 prog@(_, ((MP adr anw) : _)) pos zst@(av, lv) = inter_1 newEPS
                                                                  nextAdr
                                                                  (av, lv)
  where
    (newEPS, nextAdr) = modifyEPS (splitAt adr (uncurry (++) prog)) anw pos adr

inter_1 :: EPS -> Int -> Zustand -> Endzustand
inter_1 prog pos z | -- | trace ("POS:" ++ show pos ++ " VB: "++ show (gib_aus_Zustand z)) False = undefined
                     pos < 0 || pos >= length prog = z
                   | otherwise = exec_anw1 (splitAt pos prog) pos z


interpretiere_2 :: EPS -> Anfangszustand -> [Zwischenzustand]
interpretiere_2 [] az = [az]
interpretiere_2 prog az = inter_2 prog 0 az

exec_anw2
    :: ([Anweisung], [Anweisung])
    -> Int
    -> Zwischenzustand
    -> (EPS, Adresse, Zwischenzustand, Bool)
--exec_anw2 (b, ca : _) _ z | trace ("POS: "++ show (length b) ++ " " ++ show ca++ "\t" ++ show (gib_aus_Zustand z)) False = undefined
exec_anw2 prog@(_, ((AZ var val) : _)) pos (av, lv) =
    (uncurry (++) prog, pos + 1, (neuAB, lv), True)
    where neuAB x = if x == var then links (evaluiere val (av, lv)) else av x
exec_anw2 prog@(_, ((LZ var val) : _)) pos (av, lv) =
    (uncurry (++) prog, pos + 1, (av, neuLB), True)
    where neuLB x = if x == var then rechts (evaluiere val (av, lv)) else lv x
exec_anw2 prog@(_, ((FU val adrOK adrNOK) : _)) _ zst =
    ( uncurry (++) prog
    , if rechts (evaluiere val zst) then adrOK else adrNOK
    , zst
    , False
    )
exec_anw2 prog@(_, ((BS val adrOK) : _)) pos zst =
    ( uncurry (++) prog
    , if rechts (evaluiere val zst) then adrOK else pos + 1
    , zst
    , False
    )
exec_anw2 prog@(_, ((US adr) : _)) pos zst =
    (uncurry (++) prog, adr, zst, False)
exec_anw2 prog@(_, ((MP adr anw) : _)) pos zst = (newEPS, nextAdr, zst, False)
  where
    (newEPS, nextAdr) = modifyEPS (splitAt adr (uncurry (++) prog)) anw pos adr

inter_2 :: EPS -> Int -> Zwischenzustand -> [Zwischenzustand]
inter_2 prog pos z | nextPos < 0 || nextPos >= length prog = [z]
                   | neuerZst = (z : inter_2 eps nextPos zst)
                   | otherwise = inter_2 eps nextPos zst
    where (eps, nextPos, zst, neuerZst) = exec_anw2 (splitAt pos prog) pos z


-- A2
gib_aus_arith_Varbel :: Arith_Variablenbelegung -> [(Arith_Variable, Int)]
gib_aus_arith_Varbel ab = map (\x -> (x, ab x)) [A1 .. A6]

gib_aus_log_Varbel :: Log_Variablenbelegung -> [(Log_Variable, Bool)]
gib_aus_log_Varbel lb = map (\x -> (x, lb x)) [L1 .. L6]

gib_aus_Zustand :: Zustand -> ([(Arith_Variable, Int)], [(Log_Variable, Bool)])
gib_aus_Zustand (ab, lb) = (gib_aus_arith_Varbel ab, gib_aus_log_Varbel lb)

-- A3
ggt :: EPS
ggt =
    [ BS (Gleich (AV A1) (AK 0))  7, -- 0
      BS (Gleich (AV A1) (AV A2)) 5, -- 1
      FU (Nicht (Kleiner (AV A1) (AV A2))) 3 5, -- 2
      AZ A1 (Minus (AV A1) (AV A2)), -- 3
      US 6, -- 4
      AZ A2 (Minus (AV A2) (AV A1)), -- 5
      BS (Nicht (Gleich (AV A2) (AK 0))) 1, -- 6
      AZ A3 (AV A1) -- 7
    ]

fibo :: EPS
fibo =
    [ LZ L1 (Nicht (Kleiner (AV A1) (AK 0))), -- 0
      BS (LV L1) 3, -- 1
      AZ A1 (Minus (AK 0) (AV A1)), -- 2
      AZ A6 (AK 0), -- 3
      BS (Gleich (AV A1) (AK 0)) 77, -- 4
      AZ A6 (AK 1), -- 5
      BS (Oder (Gleich (AV A1) (AK 1)) (Gleich (AV A1) (AK 2))) 77, -- 6
      AZ A2 (AK 1), -- 7
      AZ A3 (AK 1), -- 8
      AZ A5 (AK 2), -- 9 (counter)
      AZ A5 (Plus (AV A5) (AK 1)), -- 10
      AZ A4 (Plus (AV A2) (AV A3)), -- 11
      AZ A2 (AV A3), -- 12
      AZ A3 (AV A4), -- 13
      BS (Nicht (Gleich (AV A5) (AV A1))) 10, -- 14
      AZ A6 (AV A4) -- 15
    ]

azst1 :: Anfangszustand
azst1 = generiere [24, 60] (replicate 6 True)

azst2 :: Anfangszustand
azst2 = generiere (18 : 45 : [3 .. 6]) (concat (replicate 3 [False, True]))

generiere :: [Int] -> [Bool] -> Zustand
generiere a l =
    ( \av -> case av of
        A1 -> if length a > 0 then a !! 0 else 0
        A2 -> if length a > 1 then a !! 1 else 0
        A3 -> if length a > 2 then a !! 2 else 0
        A4 -> if length a > 3 then a !! 3 else 0
        A5 -> if length a > 4 then a !! 4 else 0
        A6 -> if length a > 5 then a !! 5 else 0
    , \lv -> case lv of
        L1 -> if length l > 0 then l !! 0 else False
        L2 -> if length l > 1 then l !! 1 else False
        L3 -> if length l > 2 then l !! 2 else False
        L4 -> if length l > 3 then l !! 3 else False
        L5 -> if length l > 4 then l !! 4 else False
        L6 -> if length l > 5 then l !! 5 else False
    )
