module Angabe5 where

-- A1
type Nat0 = Int
type Nat1 = Int
data Wochentag = Mo | Di | Mi | Do | Fr | Sa | So deriving (Eq,Show,Enum)
type Starttag = Wochentag
type Zeitraum_in_Tagen = Nat1
type Streikaufrufabstand_in_Tagen = Nat1
newtype Partei = P Streikaufrufabstand_in_Tagen deriving Show
type Parteien = [Partei]
type Streiktage = Nat0
type Anzahl_Parteien = Nat1
type Modellszenario = (Starttag,Zeitraum_in_Tagen,Parteien)

streikt_partei_an t (P x)
    | mod t x == 0 = 1
    | otherwise = 0

tag_zu_zahl :: Wochentag -> Int
tag_zu_zahl = fromEnum

count_streik_an_tag :: Parteien -> Int -> Int -> Int
count_streik_an_tag ps offset tag
    | mod (tag+offset) 7 == 5 || mod (tag+offset) 7 == 0 = 0
    | otherwise = sum (map (streikt_partei_an tag) ps)

streiktage :: Modellszenario -> Streiktage
streiktage (st,zr,ps) = length (filter (>0) (map (count_streik_an_tag ps (tag_zu_zahl st)) [1..zr]))

superstreiktage :: Modellszenario -> Streiktage
superstreiktage (st,zr,ps) = length (filter (== (length ps)) (map (count_streik_an_tag ps (tag_zu_zahl st)) [1..zr]))

grossstreiktage :: Modellszenario -> Anzahl_Parteien -> Streiktage
grossstreiktage (st,zr,ps) min_anz = length (filter (>= min_anz) (map (count_streik_an_tag ps (tag_zu_zahl st)) [1..zr]))

streiktage_am :: Modellszenario -> Wochentag -> Anzahl_Parteien -> Streiktage
streiktage_am (st,zr,ps) tag min_anz = length (filter (>= min_anz) (map (count_streik_an_tag ps (tag_zu_zahl st)) [t | t <- [1..zr], mod (t+(tag_zu_zahl st)) 7 == ((tag_zu_zahl tag)+1)]))

wird_gestreikt :: Modellszenario -> Nat1 -> Bool
wird_gestreikt (st,zr,ps) nr_tag
    | nr_tag > zr = False
    | otherwise = (count_streik_an_tag ps (tag_zu_zahl st) nr_tag) > 0


-- A2
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
type Variablenbelegung = (Arith_Variablenbelegung,Log_Variablenbelegung)
links :: (Either a b) -> a
links (Left x) = x
rechts :: (Either a b) -> b
rechts (Right y) = y

class Evaluierbar a where
    evaluiere :: a -> Variablenbelegung -> Either Int Bool

instance Evaluierbar Arith_Ausdruck where
    evaluiere (AK a) (av,lv) = Left a
    evaluiere (AV a) (av,lv) = Left (av a)
    evaluiere (Plus a b) (av,lv) = Left (links (evaluiere a (av,lv)) + links (evaluiere b (av,lv)))
    evaluiere (Minus a b) (av,lv) = Left (links (evaluiere a (av,lv)) - links (evaluiere b (av,lv)))
    evaluiere (Mal a b) (av,lv) = Left (links (evaluiere a (av,lv)) * links (evaluiere b (av,lv)))

instance Evaluierbar Log_Ausdruck where
    evaluiere (LK a) (av,lv) = Right a
    evaluiere (LV a) (av,lv) = Right (lv a)
    evaluiere (Nicht a) (av,lv) = Right (not (rechts (evaluiere a (av,lv))))
    evaluiere (Und a b) (av,lv) = Right (rechts (evaluiere a (av,lv)) && rechts (evaluiere b (av,lv)))
    evaluiere (Oder a b) (av,lv) = Right (rechts (evaluiere a (av,lv)) || rechts (evaluiere b (av,lv)))
    evaluiere (Gleich a b) (av,lv) = Right (links (evaluiere a (av,lv)) == links (evaluiere b (av,lv)))
    evaluiere (Kleiner a b) (av,lv) = Right (links (evaluiere a (av,lv)) < links (evaluiere b (av,lv)))
