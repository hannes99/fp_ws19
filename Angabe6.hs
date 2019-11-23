module Angabe6 where
-- halolololodfgdh
import           Debug.Trace

data Arith_Variable = A1 | A2 | A3 | A4 | A5 | A6 deriving (Eq,Show)
data Log_Variable = L1 | L2 | L3 | L4 | L5 | L6 deriving (Eq,Show)
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
    | adr == length oldProg             = (oldProg ++ [anw], adr)
    | otherwise                         = (before ++ (anw : after), adr)
    where oldProg = uncurry (++) prog


exec_anw1 :: ([Anweisung], [Anweisung]) -> Int -> Zustand -> Endzustand
exec_anw1 prog@(before, ca : after) pos zst@(av, lv)
    | trace (" " ++ show ca) False = undefined
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
inter_1 prog pos z | trace ("POS:" ++ show pos) False = undefined
                   | pos < 0 || pos >= length prog = z
                   | otherwise = exec_anw1 (splitAt pos prog) pos z


interpretiere_2 :: EPS -> Anfangszustand -> [Zwischenzustand]
interpretiere_2 prog az = reverse (inter_2 prog 0 [az])

exec_anw2
    :: ([Anweisung], [Anweisung])
    -> Int
    -> [Zwischenzustand]
    -> [Zwischenzustand]
exec_anw2 prog@(before, ca : after) pos zst@((av, lv) : zs)
    | trace (" " ++ show ca ++ " " ++ show (length zst)) False = undefined
exec_anw2 prog@(_, ((AZ var val) : _)) pos zst@((av, lv) : zs) = inter_2
    (uncurry (++) prog)
    (pos + 1)
    ((neuAB, lv) : zst)
    where neuAB x = if x == var then links (evaluiere val (av, lv)) else av x
exec_anw2 prog@(_, ((LZ var val) : _)) pos zst@((av, lv) : zs) = inter_2
    (uncurry (++) prog)
    (pos + 1)
    ((av, neuLB) : zst)
    where neuLB x = if x == var then rechts (evaluiere val (av, lv)) else lv x
exec_anw2 prog@(_, ((FU val adrOK adrNOK) : _)) _ zst@((av, lv) : zs) = inter_2
    (uncurry (++) prog)
    (if rechts (evaluiere val (av, lv)) then adrOK else adrNOK)
    zst
exec_anw2 prog@(_, ((BS val adrOK) : _)) pos zst@((av, lv) : zs) = inter_2
    (uncurry (++) prog)
    (if rechts (evaluiere val (av, lv)) then adrOK else pos + 1)
    zst
exec_anw2 prog@(_, ((US adr) : _)) pos zst@((av, lv) : zs) =
    inter_2 (uncurry (++) prog) adr zst
exec_anw2 prog@(_, ((MP adr anw) : _)) pos zst@((av, lv) : zs) = inter_2
    newEPS
    nextAdr
    zst
  where
    (newEPS, nextAdr) = modifyEPS (splitAt adr (uncurry (++) prog)) anw pos adr

inter_2 :: EPS -> Int -> [Zwischenzustand] -> [Zwischenzustand]
inter_2 prog pos z | trace ("POS:" ++ show pos) False = undefined
                   | pos < 0 || pos >= length prog = z
                   | otherwise = exec_anw2 (splitAt pos prog) pos z
