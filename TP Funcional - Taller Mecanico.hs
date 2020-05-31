module TallerDelMecanico where

import Text.Show.Functions()

type Desgaste = Float
type Patente  = String
type Fecha    = (Int, Int, Int)

-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year

data Auto = UnAuto {
 patente         :: Patente,
 desgasteLlantas :: [Desgaste],
 rpm             :: Int,
 temperaturaAgua :: Int,
 ultimoArreglo   :: Fecha
} deriving (Show, Eq)

honda :: Auto
honda = UnAuto "AT001LN" [0.5, 0.1, 0.0, 0.2] 2500 95 (25, 10, 2019)

fiat :: Auto
fiat  = UnAuto "DJV214" [0.50, 0.1, 0.6, 0.4] 1500 90 (04, 05, 2016)

ford :: Auto
ford  = UnAuto "DFH029" [0.51, 0.1, 0.6, 0.4] 1900 95 (25, 10, 2015)

-- Punto 1)
 
costoDeReparacion :: Auto -> Int
costoDeReparacion auto | cantidadDigitos 7 laPatente = 12500
                       | patenteEstaEntre "DJ" "NB" laPatente = calculoPatental laPatente
                       | otherwise = 15000
                       where laPatente = patente auto

cantidadDigitos :: Int -> String -> Bool
cantidadDigitos cantidad = (== cantidad) . length

patenteEstaEntre :: String -> String -> String -> Bool
patenteEstaEntre inferior superior = estaEntrePalabras inferior superior . take 2

estaEntrePalabras :: String -> String -> String -> Bool
estaEntrePalabras primerPalabra ultimaPalabra palabra = primerPalabra <= palabra && palabra <= ultimaPalabra

terminaEn :: Char -> String -> Bool
terminaEn terminacion = (== terminacion) . last

calculoPatental :: Patente -> Int
calculoPatental patent | terminaEn '4' patent = 3000 * length patent
                       | otherwise = 20000


-- Punto 2) A)

autoPeligroso :: Auto -> Bool
autoPeligroso = (> 0.5) . head . desgasteLlantas

-- Punto 2) B)

necesitaRevision :: Auto -> Bool
necesitaRevision = (<= 2015) . anio . ultimoArreglo


-- Punto 3)
-- Alfa : hace que el auto regule a 2.000 vueltas, salvo que esté a menos de 2.000 vueltas,
-- en cuyo caso lo deja como está
mapDesgasteLlantas :: ([ Desgaste ] -> [ Desgaste ]) -> Auto -> Auto
mapDesgasteLlantas unaFuncion auto = auto { desgasteLlantas = unaFuncion . desgasteLlantas $ auto }

alfa :: Tecnico
alfa auto = auto { rpm = min (rpm auto) 2000}

-- Bravo : cambia todas las cubiertas, dejándolas sin desgaste
bravo :: Tecnico
bravo auto = mapDesgasteLlantas (map (*0)) auto      --bravo auto = auto { desgasteLlantas = [0.0, 0.0, 0.0, 0.0] }

-- Charly : realiza las mismas actividades que Alfa y Bravo
charly :: Tecnico
charly = bravo.alfa

-- Tango : le gusta decir que hizo muchas cosas pero en realidad no hace ningún arreglo
tango :: Tecnico
tango = id

-- Zulu : revisa la temperatura del agua, la deja a 90 y hace lo mismo que Lima (ver a continuación)
zulu :: Tecnico
zulu auto = lima auto { temperaturaAgua = 90}

-- Lima : cambia las cubiertas delanteras (las dos primeras),
-- dejándolas sin desgaste. Las posteriores quedan igual

lima :: Tecnico
lima auto = mapDesgasteLlantas (ponerLlanta . quitarLlanta) auto   --lima auto = auto { desgasteLlantas = (ponerLlanta.ponerLlanta.quitarLlanta.quitarLlanta.desgasteLlantas) auto }

quitarLlanta :: [Desgaste] -> [Desgaste]
quitarLlanta = drop 2

ponerLlanta :: [Desgaste] -> [Desgaste]
ponerLlanta = ([0,0] ++)


-- Punto 4)
-- autoPrueba1, autoPrueba2 y autoPrueba3 son para probar el punto 4)

autoPrueba1 :: Auto
autoPrueba1 = UnAuto "DJV215" [0.1, 0.3, 0.2, 0.1] 1500 90 (04, 05, 2016)

autoPrueba2 :: Auto
autoPrueba2 = UnAuto "AT001LN" [0.3, 0.5, 0.6, 0.2] 1500 90 (04, 05, 2015)

autoPrueba3 :: Auto
autoPrueba3 = UnAuto "DFH029" [0.1, 0.1, 0.1, 0] 1500 90 (04, 05, 2014)

--type Autos = [ Auto ]  -- Sacarlo
pruebaListaAutos :: [ Auto ]
pruebaListaAutos = [autoPrueba1, autoPrueba2, autoPrueba3]

pruebaListaAutos2 :: [ Auto ]             -- es una lista infinita
pruebaListaAutos2 = cycle pruebaListaAutos      -- pruebaListaAutos2 = autoPrueba1 : autoPrueba2 : autoPrueba3 : pruebaListaAutos2


ordenamientoTOC :: [ Auto ] -> Bool
ordenamientoTOC [] = True
ordenamientoTOC [auto] = odd (cantidadDesgaste auto)
ordenamientoTOC (auto1 : auto2 : autos) = odd (cantidadDesgaste auto1) && even (cantidadDesgaste auto2) && ordenamientoTOC autos

cantidadDesgaste :: Auto -> Int
cantidadDesgaste  = round . (*10) . sum . desgasteLlantas

-- Punto 5)

type Tecnico = Auto -> Auto
pruebaListaTecnicos :: [ Tecnico ]
pruebaListaTecnicos = [ alfa, bravo, charly, tango, zulu, lima ]

pruebaListaTecnicos2 :: [ Tecnico ]                         -- lista infinita
pruebaListaTecnicos2 = cycle pruebaListaTecnicos


--type Mecanico = Auto -> Auto

type Orden = (Fecha, [Tecnico])

actualizarUltimoArreglo :: Fecha -> Auto -> Auto
actualizarUltimoArreglo fecha auto = auto { ultimoArreglo = fecha }   

reparar :: Auto -> [Tecnico] -> Auto
reparar auto tecnicos = foldr ($) auto tecnicos

ordenDeReparación :: Orden -> Auto -> Auto
ordenDeReparación (fecha, tecnicos) auto = actualizarUltimoArreglo fecha (reparar auto tecnicos )

--_________________________________________________________________________________________

-- Punto 6) Parte 1)
-- Dada una lista de técnicos determinar aquellos técnicos que dejarían el auto en condiciones,
-- es decir que no sea peligroso andar.

listaTecnicosAutoEnCondiciones :: [Tecnico] -> Auto -> [Tecnico]       -- listado de tecnicos que hacen que deje de ser peligroso un auto
listaTecnicosAutoEnCondiciones tecnicos auto = filter (not . autoPeligroso . ($ auto)) tecnicos

-- Punto 6) Parte 2)
-- Dada una lista de autos, saber cuál es el costo de reparación de los autos que necesitan revisión.

costoReparacionNecesitaRevision :: [ Auto ] -> Int
costoReparacionNecesitaRevision autos = ( sum . map costoDeReparacion . filter necesitaRevision) autos

-- Punto 7) Parte 1)
{-
  Si podriamos obtener el primer tecnico  que deja el auto en condiciones, si aplicamos la funcion head a
la funcion listaTecnicosAutoEnCondiciones

  ejemplo: (pruebaListaTecnicos3 es una lista infinita)

*TallerDelMecanico> (head . listaTecnicosAutoEnCondiciones pruebaListaTecnicos2) ford
-}
-- Punto 7) Parte 2)
{-
  No podremos saber cual seria el costo de reparacion de los autos que necesitan revision de una lista infinita
de autos dado que tendriamos que saber de antemano la totalidad de los autos que necesitan revision, y siendo
una lista infinita siempre podrian aparecer autos que necestien revision.

ejemplo: (pruebaListaAutos2 es una lista infinita)
*TallerDelMecanico> costoReparacionNecesitaRevision pruebaListaAutos2      -- apretamos CTRL + C para interrumpir el ciclo infinito
Interrupted.

  En el caso de tomar en cuenta los 3 primeros autos que necesitan revision la funcion deberia cambiar a:

-}

-- Punto 6) Parte 2) Modificada para el punto 7) Parte 2)

costoReparacionNecesitaRevision2 :: [ Auto ] -> Int
costoReparacionNecesitaRevision2 autos = (sum . map costoDeReparacion . take 3 . filter necesitaRevision) autos

