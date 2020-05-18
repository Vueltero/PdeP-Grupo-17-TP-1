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

-- Dejo este Auto para que se realicen pruebas sin necesidad de definir por consola

autoDePrueba = Auto "DFH029" [ 0.5, 0.1, 0.1, 0.2 ] 2001 80 (27, 10, 1997) 



--Punto 1 - "Costo de reparación de un auto"

estaEntreDJyNB :: Patente-> Bool
estaEntreDJyNB patente = take 2 patente >= "DJ" && take 2 patente <= "NB"

calculoPatental :: Patente -> Int
calculoPatental patente
 | (last patente) == '4' = (length patente) * 3000
 | otherwise = 20000

costoReparacion :: Auto -> Int
costoReparacion auto
 | (length patenteAuto == 7) = 12500
 | estaEntreDJyNB patenteAuto = calculoPatental patenteAuto
 | otherwise = 15000
 where patenteAuto = patente auto

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

alfa :: Auto -> Auto 
alfa auto = regularAuto auto 
 where regularAuto auto | (rpm auto) < 2000 = auto 
                        | otherwise = auto {rpm = 2000}

bravo :: Auto -> Auto 
bravo auto = cambiarLlantas auto 
 where cambiarLlantas auto = auto { desgasteLlantas = [0.0, 0.0, 0.0, 0.0 ]}

charly :: Auto -> Auto
charly auto = (alfa.bravo) auto 

tango :: Auto -> Auto
tango auto = auto 

lima :: Auto -> Auto
lima auto = cambiarLlantasDelanteras auto 
 where cambiarLlantasDelanteras auto = auto { desgasteLlantas = (0.0 : 0.0 : drop 2 (desgasteLlantas auto))}

zulu :: Auto -> Auto
zulu auto = (cambiarTemperaturaAguaA90.lima) auto 
 where cambiarTemperaturaAguaA90 auto = auto { temperaturaAgua = 90}

 -------------------------------------------------------


{- Punto 4: Ordenamiento TOC de autos

Dada una serie de autos, saber si están ordenados en base al siguiente criterio:
⦁	los autos ubicados en la posición impar de la lista deben tener una cantidad de desgaste impar
⦁	los autos ubicados en la posición par de la lista deben tener una cantidad de desgaste par

Nota: asumimos que el primer elemento está en la posición 1, el segundo elemento en la posición 2, etc.

La cantidad de desgaste es la sumatoria de desgastes de las cubiertas de los autos multiplicada por 10. 
Ejemplo: 0.2 + 0.5 + 0.6 + 0.1 = 1.4 * 10 = 14. Para determinar si es par o no (y evitar errores de redondeo) es conveniente utilizar la función round.

-}

siSoloSi :: Bool -> Bool -> Bool
siSoloSi True True = True
siSoloSi False False = True
siSoloSi _ _ = False

estaOrdenado :: Int -> [Int] -> Bool
estaOrdenado 1 (x:xs) = odd x
estaOrdenado indice (x:xs) = siSoloSi (even indice) (even x) && estaOrdenado (indice - 1) xs

estaOrdenadoToc :: [Auto] -> Bool
estaOrdenadoToc autos = estaOrdenado (length autos) ((reverse.map (round.(*10).sum.desgasteLlantas)) autos)

-------------------------------------------------------

{- Punto 5: Orden de reparación

Aplicar una orden de reparación, que tiene
⦁	una fecha
⦁	una lista de técnicos
y consiste en que cada uno de los técnicos realice las reparaciones que sabe sobre el auto, al que además hay que actualizarle la última fecha de reparación.

-}

type Mecanico = Auto -> Auto
type Orden = (Fecha, [Mecanico])

fcAux ::  Auto -> Mecanico -> Auto 
fcAux auto x = x auto

actualizarUltimoArreglo :: Fecha -> Auto -> Auto
actualizarUltimoArreglo fecha auto = auto { ultimoArreglo = fecha }   

reparar :: Auto -> [Mecanico] -> Auto
reparar auto lista = foldl fcAux auto lista

ordenDeReparación :: Orden -> Auto -> Auto
ordenDeReparación (fecha, lista) auto = actualizarUltimoArreglo fecha (reparar auto lista )
