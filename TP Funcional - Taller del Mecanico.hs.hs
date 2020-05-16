module TallerDelMecanico where


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

-- Punto 1 - "Costo de reparación de un auto"

{-●	si la patente tiene 7 dígitos, es $ 12.500

●	si no, si la patente está entre las
	letras "DJ" y "NB"  , se aplica el calculoPatental
	○	que es $ 3.000 * la longitud para
		las patentes que terminen en 4
	○	o $ 20.000 para el resto de las patentes

●	de lo contrario, se le cobra $ 15000

Los strings ("DJ", "NB") deben compararse con
(<), (<=), (>), (>=)-}

--estaEntreDJyNB :: Patente -> Bool
--estaEntreDJyNB = ((>= "DJ").(take 2)) patente && ((<= "NB").(take 2)) patente

estaEntreDJyNB :: Patente -> Bool
estaEntreDJyNB patente = (entreDJ patente && entreNB patente)
    where entreDJ = ((>= "DJ").(take 2))
          entreNB = ((<= "NB").(take 2))

terminenEn4 :: Patente -> Bool
terminenEn4 = ultimoDigitoEs4
    where ultimoDigitoEs4 = (== "4").(drop 5)

costoReparacion :: Patente -> Int
costoReparacion patente
 | (length patente == 7) = 12500
 | estaEntreDJyNB patente &&  terminenEn4 patente = 3000 * 6
 | estaEntreDJyNB patente = 20000
 | otherwise = 15000

-------------------------------------------------------

{- Punto 2

A) Auto peligroso.
Dado un auto, saber si es peligroso. 
Esta condición se cumple cuando el desgaste de la primera llanta es mayor a 0.5

B) Necesita revisión.
Dado un auto, saber si necesita revisión. 
Esta condición se cumple cuando el último arreglo fue realizado en el año 2015 ó antes. -}


autoPeligroso :: [Desgaste] -> Bool
autoPeligroso = esPeligroso
    where esPeligroso = (> 0.5).head

necesitaRevision :: Fecha -> Bool
necesitaRevision = necesitaArreglo
    where necesitaArreglo = (<= 2015).anio

-------------------------------------------------------

-- Punto 3 - "Personal técnico encargado de las reparaciones"

{- Necesitamos definir a las siguientes personas que realizan actividades en el taller mecánico:
●	Alfa: hace que el auto regule a 2.000 vueltas, salvo que esté a menos de 2.000 vueltas, en cuyo caso lo deja como está.
●	Bravo: cambia todas las cubiertas, dejándolas sin desgaste.
●	Charly:  realiza las mismas actividades que Alfa y Bravo.
●	Tango: le gusta decir que hizo muchas cosas pero en realidad no hace ningún arreglo.
●	Zulu: revisa la temperatura del agua, la deja a 90 y hace lo mismo que Lima (ver a continuación).
●	Lima:  cambia las cubiertas delanteras (las dos primeras), dejándolas sin desgaste. Las posteriores quedan igual. -}

