{-Todo auto tiene
●	la patente, que puede tener formato viejo "RVM363"
o el nuevo "AB808RD"

●	el desgaste de cada una de las llantas,
ej: [ 0.5, 0.1, 0.0, 0.2 ]

●	las revoluciones por minuto a las que regula el
motor, ej: 1500

●	la temperatura del agua luego de 5 minutos
de encendido el auto: 90

●	la fecha del último arreglo-}

-------------------------------------------------------

type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)

-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year

data Auto = Auto {
 patente         :: Patente,
 desgasteLlantas :: [Desgaste],
 rpm             :: Int,
 temperaturaAgua :: Int,
 ultimoArreglo   :: Fecha
} deriving (Show, Eq)

-------------------------------------------------------

--Punto 1 - "Costo de reparación de un auto"

estaEntreDJyNB :: Patente -> Bool
estaEntreDJyNB patente =
 take 2 patente >= "DJ" && take 2 patente <= "NB"

terminenEn4 :: Patente -> Bool
terminenEn4 patente = drop 5 patente == "4"

costoReparacion :: Patente -> Int
costoReparacion patente
 | (length patente == 7) = 12500
 | estaEntreDJyNB patente && terminenEn4 patente = 3000*6
 | estaEntreDJyNB patente = 20000
 | otherwise = 15000

-------------------------------------------------------

--Punto 2 - ""

