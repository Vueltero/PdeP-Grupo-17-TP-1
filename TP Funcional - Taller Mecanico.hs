module TallerDelMecanico where

import Text.Show.Functions()
import Data.List

type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)

-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year

data Auto = UnAuto {
 patente :: Patente,
 desgasteLlantas :: [Desgaste],
 rpm :: Int,
 temperaturaAgua :: Int,
 ultimoArreglo :: Fecha
} deriving (Show, Eq)

honda :: Auto
honda = UnAuto {
 patente = "AT001LN",
 desgasteLlantas = [0.5, 0.1, 0.0, 0.2],
 rpm = 2500,
 temperaturaAgua = 95,
 ultimoArreglo = (25, 10, 2019)
}

fiat :: Auto
fiat = UnAuto {
 patente = "DJV214",
 desgasteLlantas = [0.50, 0.1, 0.6, 0.4],
 rpm = 1500,
 temperaturaAgua = 90,
 ultimoArreglo = (04, 05, 2016)
}

ford :: Auto
ford = UnAuto {
 patente = "DFH029",
 desgasteLlantas = [0.51, 0.1, 0.6, 0.4],
 rpm = 1900,
 temperaturaAgua = 95,
 ultimoArreglo = (25, 10, 2015)
}

-- Punto 1)

costoDeReparacion :: Auto -> Int
costoDeReparacion auto | cantidadDigitos 7 (patente auto) = 12500
                       | entreDJyNB auto = calculoPatental (patente auto)
                       | otherwise = 15000

cantidadDigitos :: Int -> String -> Bool
cantidadDigitos cantidad = (== cantidad) . length

entreDJyNB :: Auto -> Bool
entreDJyNB auto = estaEntrePalabras "DJ" ((take 2.patente) auto) "NB" 

estaEntrePalabras :: String -> String -> String -> Bool
estaEntrePalabras primerPalabra palabra ultimaPalabra = primerPalabra <= palabra && palabra <= ultimaPalabra

terminaEn :: Char -> String -> Bool
terminaEn terminacion = (== terminacion).last

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

alfa :: Auto -> Auto
alfa auto = auto { rpm = min (rpm auto) 2000}

-- Bravo : cambia todas las cubiertas, dejándolas sin desgaste
bravo :: Auto -> Auto
bravo auto = mapDesgasteLlantas (map (*0)) auto     -- bravo auto = auto { desgasteLlantas = [0.0, 0.0, 0.0, 0.0] }

-- Charly : realiza las mismas actividades que Alfa y Bravo
charly :: Auto -> Auto
charly = bravo.alfa

-- Tango : le gusta decir que hizo muchas cosas pero en realidad no hace ningún arreglo
tango :: Auto -> Auto
tango = id

-- Zulu : revisa la temperatura del agua, la deja a 90 y hace lo mismo que Lima (ver a continuación)
zulu :: Auto -> Auto
zulu auto = lima auto { temperaturaAgua = 90}

-- Lima : cambia las cubiertas delanteras (las dos primeras),
-- dejándolas sin desgaste. Las posteriores quedan igual

lima :: Auto -> Auto
lima auto = mapDesgasteLlantas (ponerLlanta.ponerLlanta.quitarLlanta.quitarLlanta) auto  -- lima auto = auto { desgasteLlantas = (ponerLlanta.ponerLlanta.quitarLlanta.quitarLlanta.desgasteLlantas) auto }

quitarLlanta :: [Desgaste] -> [Desgaste]
quitarLlanta = drop 1

ponerLlanta :: [Desgaste] -> [Desgaste]
ponerLlanta = (0:)


-- Punto 4)
-- autoPrueba1, autoPrueba2 y autoPrueba3 son para probar el punto 4)

autoPrueba1 :: Auto
autoPrueba1 = UnAuto {
 patente = "DJV215",
 desgasteLlantas = [0.1, 0.3, 0.2, 0.1],
 rpm = 1500,
 temperaturaAgua = 90,
 ultimoArreglo = (04, 05, 2016)
}

autoPrueba2 :: Auto
autoPrueba2 = UnAuto {
 patente = "AT001LN",
 desgasteLlantas = [0.3, 0.5, 0.6, 0.2],
 rpm = 1500,
 temperaturaAgua = 90,
 ultimoArreglo = (04, 05, 2015)
}

autoPrueba3 :: Auto
autoPrueba3 = UnAuto {
 patente = "DFH029",
 desgasteLlantas = [0.1, 0.1, 0.1, 0],
 rpm = 1500,
 temperaturaAgua = 90,
 ultimoArreglo = (04, 05, 2014)
}

ordenamientoTOC [] = True
ordenamiento [auto] = odd (cantidadDesgaste auto)
ordenamientoTOC (auto1:auto2:autos) =
 odd (cantidadDesgaste auto1) && even (cantidadDesgaste auto2)
 && ordenaminetoTOC autos

-- Punto 5)

data Tecnico = UnTecnico {
  nombre :: String,
  reparacion :: Auto -> Auto
} deriving (Show)

tecnicoAlfa :: Tecnico
tecnicoAlfa = UnTecnico {
 nombre = "Alfa",
 reparacion = alfa
}

tecnicoBravo :: Tecnico
tecnicoBravo = UnTecnico {
 nombre = "Bravo",
 reparacion = bravo
}

tecnicoCharly :: Tecnico
tecnicoCharly = UnTecnico {
 nombre = "Charly",
 reparacion = charly
}

tecnicoTango :: Tecnico
tecnicoTango = UnTecnico {
 nombre = "Tango",
 reparacion = tango
}

tecnicoZulu :: Tecnico
tecnicoZulu = UnTecnico {
 nombre = "Zulu",
 reparacion = zulu
}

tecnicoLima :: Tecnico
tecnicoLima = UnTecnico {
 nombre = "Lima",
 reparacion = lima
}

type ListaTecnicos = [Tecnico]

pruebaListaTecnicos :: ListaTecnicos
pruebaListaTecnicos = [tecnicoAlfa, tecnicoBravo, tecnicoCharly, tecnicoTango, tecnicoZulu, tecnicoLima]

pruebaListaTecnicos2 :: ListaTecnicos
pruebaListaTecnicos2 = [tecnicoAlfa, tecnicoTango, tecnicoZulu, tecnicoLima]

pruebaListaTecnicos3 :: ListaTecnicos                         -- lista infinita
pruebaListaTecnicos3 = tecnicoAlfa : tecnicoTango : tecnicoZulu : tecnicoLima : pruebaListaTecnicos3

ordenDeReparacion :: Fecha -> ListaTecnicos -> Auto -> Auto
ordenDeReparacion fecha tecnicos = (modificarFecha fecha . reparacionTecnicos tecnicos)

reparacionTecnicos :: ListaTecnicos -> Auto -> Auto             -- conjunto de tecnicos reparando un auto
reparacionTecnicos  tecnicos auto = foldr ($) auto (tecnicoReparando tecnicos)

tecnicoReparando :: ListaTecnicos -> [Auto -> Auto]           -- lista que contiene tecnicos reparando
tecnicoReparando tecnicos = map reparacion tecnicos

modificarFecha :: Fecha -> Auto -> Auto
modificarFecha fecha auto = auto { ultimoArreglo = fecha }

-- Punto 6) Parte 1)
-- Dada una lista de técnicos determinar aquellos técnicos que dejarían el auto en condiciones,
-- es decir que no sea peligroso andar.

nombreDeTecnicosAutoEnCondiciones :: ListaTecnicos -> Auto -> [String]      -- nombres de tecnicos que hacen que deje de ser peligroso un auto
nombreDeTecnicosAutoEnCondiciones tecnicos auto = map nombre ( listaTecnicosAutoEnCondiciones tecnicos auto)

listaTecnicosAutoEnCondiciones :: ListaTecnicos -> Auto -> ListaTecnicos       -- listado de tecnicos que hacen que deje de ser peligroso un auto
listaTecnicosAutoEnCondiciones lista auto = map (lista !!) (indicesAutoEnCondiciones auto lista)

indicesAutoEnCondiciones :: Auto -> ListaTecnicos -> [Int]
indicesAutoEnCondiciones auto tecnicos = findIndices (not.autoPeligroso) (cadaTecnicoUnAuto tecnicos auto)

cadaTecnicoUnAuto :: ListaTecnicos -> Auto -> ListaAutos
cadaTecnicoUnAuto tecnicos auto = map ($ auto) (tecnicoReparando tecnicos)         -- lista de cada tecnico trabajando en el auto

-- Punto 6) Parte 2)
-- Dada una lista de autos, saber cuál es el costo de reparación de los autos que necesitan revisión.

costoReparacionNecesitaRevision :: ListaAutos -> [Int]            -- listado de costos de autos que necesitan revision
costoReparacionNecesitaRevision lista = map (costoDeReparacion) (listaNecesitaRevision lista)

listaNecesitaRevision :: ListaAutos -> ListaAutos
listaNecesitaRevision lista = map (lista !!) (indicesAutoNecesitaRevision lista)

indicesAutoNecesitaRevision :: ListaAutos -> [Int]
indicesAutoNecesitaRevision listaAutos = findIndices necesitaRevision listaAutos

-- Punto 7) Parte 1)
{-
  Si podriamos obtener el primer tecnico  que deja el auto en condiciones, si agreamos take 1 a
la funcion indicesAutoEnCondiciones (que busca los indices de la lista que dice si el auto esta en condicones,
entonces delimitamos la busqueda al primer encuento)

indicesAutoEnCondiciones :: Auto -> ListaTecnicos -> [ Int ]
indicesAutoEnCondiciones auto tecnicos = take 1 ( findIndices (not.autoPeligroso) (cadaTecnicoUnAuto tecnicos auto) )

  ejemplo: (pruebaListaTecnicos3 es una lista infinita)

*Main> nombreDeTecnicosAutoEnCondiciones pruebaListaTecnicos3 ford 
["Zulu"]
-}
-- Punto 7) Parte 2)
{-
  No podremos saber cual seria el costo de reparacion de los autos que necesitan revision de una lista infinita
de autos dado que tendriamos que saber de antemano la totalidad de los autos que necesitan revision, y siendo
una lista infinita siempre podrian aparecer autos que necestien revision.
  En el caso de tomar en cuenta los 3 primeros autos que necesitan revision la funcion deberia cambiar a:

indicesAutoNecesitaRevision :: ListaAutos -> [ Int ]
indicesAutoNecesitaRevision listaAutos = take 3 ( findIndices necesitaRevision listaAutos)

Esta version funcion acepta una lista infinita.
-}

-- Punto 6) Parte 2) Modificada para el punto 7) Parte 2)

costoReparacionNecesitaRevision2 :: ListaAutos -> [Int]            -- listado de costos de autos que necesitan revision
costoReparacionNecesitaRevision2 lista = map (costoDeReparacion) (listaNecesitaRevision2 lista)

listaNecesitaRevision2 :: ListaAutos -> ListaAutos
listaNecesitaRevision2 lista = map (lista !!) (indicesAutoNecesitaRevision2 lista)

indicesAutoNecesitaRevision2 :: ListaAutos -> [Int]
indicesAutoNecesitaRevision2 listaAutos = take 3 (findIndices necesitaRevision listaAutos)

