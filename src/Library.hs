{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Guantelete = Guantelete {
    material :: String,
    gemas    :: [String] 
} deriving Show

data Personaje = Personaje {
    edad           :: Number,
    energia        :: Energia,
    habilidad    :: Habilidad,
    nombre         :: String,
    vivenEnPlaneta :: Planeta
} deriving Show

type Energia = Number
type Habilidad = [String]
type Planeta = String
type Universo = [Personaje]

guanteleteCompleto :: Guantelete -> Bool-- Funcion auxiliar
guanteleteCompleto guantelete = ((== 6) . length . gemas) guantelete && ((=="uru") . material) guantelete

chasquearUniverso :: Guantelete -> Universo -> Universo
chasquearUniverso guantelete universo 
    | guanteleteCompleto guantelete = take (length universo `div` 2) universo
    | otherwise                     = universo

-- Resolver utilizando únicamente orden superior.
-- Saber si un universo es apto para péndex, que ocurre si alguno de los personaje
-- que lo integran tienen menos de 45 años.
aptoParaPendex :: Universo -> Bool
aptoParaPendex personaje = any ((< 45) . edad) personaje

-- Saber la energía total de un universo que es la sumatoria de todas las energías de
-- sus integrantes que tienen más de una habilidad
energiaTotalUniverso :: Universo -> Energia
energiaTotalUniverso personaje = sum (map energia (filter ((> 1) . length . habilidad) personaje))


type Gema = Personaje -> Personaje

-- laMente tiene la habilidad de debilitar la energía de un usuario en un valor dado
laMente :: Number -> Gema
laMente cuantaEnergiaSaca personaje = personaje { energia = ((cuantaEnergiaSaca-) . energia) personaje }

-- elAlma permite eliminar una habilidad en particular si es que la posee. Además le quita 10 puntos de energía.
elAlma :: String -> Gema
elAlma habilidadAQuitar personaje = personaje { energia     = ((10-) . energia) personaje,
                                                habilidad = filter (/= habilidadAQuitar) (habilidad personaje) }

-- elEspacio transporta al rival al planeta x (el que usted decida) y resta 20 puntos de energía
elEspacio :: Planeta -> Gema
elEspacio planetaX personaje = personaje { vivenEnPlaneta = planetaX,
                                           energia = ((20-) . energia) personaje }

-- elPoder deja sin energía al rival y si tiene <=2 habilidad se las quita (en caso contrario no le saca ninguna habilidad).
elPoder :: Gema
elPoder personaje
    | length (habilidad personaje) <= 2 = personaje { energia = 0, habilidad = [] }
    | otherwise = personaje

-- elTiempo reduce a la mitad entera la edad de su oponente pero no puede dejar la edad del oponente con menos de 18 años. 
-- También resta 50 puntos de energía.
elTiempo :: Gema
elTiempo personaje = personaje { edad    = edad personaje `div` 2,
                                 energia = ((50-) . energia) personaje }

-- La gema loca que permite manipular el poder de una gema y la ejecuta 2 veces contra un rival
laGemaLoca :: Gema -> Gema
laGemaLoca gema = gema . gema


aplicarGemas :: [Gema] -> Personaje -> Personaje
aplicarGemas gemas personaje = foldl (flip ($)) personaje gemas

personajePrueba = Personaje {
    edad           = 50,
    energia        = 1000,
    habilidad    = ["usar Mjolnir", "programación en Haskell"],
    nombre         = "Goncho",
    vivenEnPlaneta = "X"
}
{-
> aplicarGemas [laGemaLoca elTiempo . elTiempo . elPoder . elEspacio ("Tierra") . elAlma . laMente 30] personajePrueba
Personaje
    { edad = 6
    , energia = 50
    , habilidad = []
    , nombre = "Goncho"
    , vivenEnPlaneta = "Tierra"
    }
-}

-- Dar un ejemplo de un guantelete de goma con las gemas tiempo, alma que quita la habilidad de “usar Mjolnir” 
-- y la gema loca que manipula el poder del alma tratando de eliminar la “programación en Haskell"
guanteleteDeGoma = Guantelete {
    material = "goma",
    gemas    = ["Tiempo", "Alma", "GemaLoca"]
}

guanteleteDeGomaConGemas :: Guantelete -> Personaje -> Personaje
guanteleteDeGomaConGemas guantelete = 
    aplicarGemas [elAlma "usar Mjolnir" . laGemaLoca (elAlma "programación en Haskell")]

-- Resolver utilizando recursividad. 
-- Definir la función gemaMasPoderosa que dado un guantelete y una persona obtiene la gema del infinito que
-- produce la pérdida más grande de energía sobre la persona
--gemaMasPoderosa :: Guantelete -> Personaje -> Gema
--gemaMasPoderosa 


{-
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas tiempo)

Y la función

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete2

Justifique si se puede ejecutar, relacionándolo con conceptos vistos en la cursada:
● gemaMasPoderosa punisher guanteleteDeLocos3 

● usoLasTresPrimerasGemas guanteleteDeLocos punisher
-}