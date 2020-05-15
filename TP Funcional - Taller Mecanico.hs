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

data Auto = Auto
{
	patente         :: Patente,
	desgasteLlantas :: [Desgaste],
	rpm             :: Int,
	temperaturaAgua :: Int,
	ultimoArreglo   :: Fecha
} deriving (Show, Eq)

-------------------------------------------------------

--Punto 1 - "Costo de reparación de un auto"

{-●	si la patente tiene 7 dígitos, es $ 12.500

●	si no, si la patente está entre las
	letras "DJ" y "NB"  , se aplica el calculoPatental
	○	que es $ 3.000 * la longitud para
		las patentes que terminen en 4
	○	o $ 20.000 para el resto de las patentes

●	de lo contrario, se le cobra $ 15000

Los strings ("DJ", "NB") deben compararse con
(<), (<=), (>), (>=)-}

--costoReparacion :: Auto -> Int
costoReparacion patente
	| (length patente = 7) = 12500
	| 
	| otherwise = 15000