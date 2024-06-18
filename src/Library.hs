{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Guantelete = Guantelete {
    material :: String,
    gemas    :: [Gema] 
} deriving Show
data Personaje = Personaje {
    edad           :: Number,
    energia        :: Energia,
    habilidad      :: Habilidad,
    nombre         :: String,
    vivenEnPlaneta :: Planeta
} deriving Show

type Energia = Number
type Habilidad = [String]
type Planeta = String
type Universo = [Personaje]

guanteleteCompleto :: Guantelete -> Bool -- Funcion auxiliar
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
energiaTotalUniverso = sum . map energia . filter ((> 1) . length . habilidad)
-- con map transformas la lista de personajes en una lista de energias

type Gema = Personaje -> Personaje

-- laMente tiene la habilidad de debilitar la energía de un usuario en un valor dado
laMente :: Number -> Gema
laMente = quitarEnergia

quitarEnergia :: Number -> Personaje -> Personaje -- Funcion auxiliar
quitarEnergia cuantaEnergiaSaca personaje = personaje { energia = energia personaje - cuantaEnergiaSaca }


-- elAlma permite eliminar una habilidad en particular si es que la posee. Además le quita 10 puntos de energía.
elAlma :: String -> Gema
elAlma habilidadAQuitar personaje = quitarEnergia 10 personaje { 
    habilidad = filter (/= habilidadAQuitar) $ habilidad personaje
}

-- elEspacio transporta al rival al planeta x (el que usted decida) y resta 20 puntos de energía
elEspacio :: Planeta -> Gema
elEspacio planetaX personaje = quitarEnergia 20 personaje { vivenEnPlaneta = planetaX }

-- elPoder deja sin energía al rival y si tiene <=2 habilidad se las quita (en caso contrario no le saca ninguna habilidad).
elPoder :: Gema
elPoder personaje = atacarHabilidades . quitarEnergia (energia personaje) $ personaje

atacarHabilidades :: Personaje -> Personaje -- Funcion auxiliar
atacarHabilidades personaje 
    | (<=2) . length . habilidad $ personaje = quitarHabilidades personaje
    | otherwise = personaje   

quitarHabilidades:: Gema -- Funcion auxiliar
quitarHabilidades personaje = personaje { habilidad = [] }
-- elTiempo reduce a la mitad entera la edad de su oponente pero no puede dejar la edad del oponente con menos de 18 años. 
-- También resta 50 puntos de energía.
elTiempo :: Gema
elTiempo personaje = quitarEnergia 50 personaje { edad = max 18 (edad personaje `div` 2) }

-- La gema loca que permite manipular el poder de una gema y la ejecuta 2 veces contra un rival
laGemaLoca :: Gema -> Gema
laGemaLoca gema = gema . gema

 
aplicarGemas :: [Gema] -> Gema
aplicarGemas listaGemas enemigo = foldl (flip ($)) enemigo listaGemas

personajePrueba = Personaje {
    edad           = 50,
    energia        = 1000,
    habilidad      = ["usar Mjolnir", "programación en Haskell"],
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
unGuantelete = Guantelete {
    material = "goma",
    gemas    = [elTiempo, elAlma "usar Mjolnir", laGemaLoca (elAlma "programación en Haskell")]
}

-- Generar la función utilizar que dado una lista de gemas y un enemigo 
-- ejecuta el poder de cada una de las gemas que lo componen contra el personaje dado. 
utilizar :: [Gema] -> Gema
utilizar listaGemas enemigo = foldr ($) enemigo listaGemas
-- Indicar cómo se produce el “efecto de lado” sobre la víctima.
{-
En haskell no hay “efecto de lado” sino  que ese personaje se ve afectado por la gema y 
se crea un nuevo personaje con las características modificadas.
-}


-- Resolver utilizando recursividad. 
-- Definir la función gemaMasPoderosa que dado un guantelete y una persona obtiene la gema del infinito que
-- produce la pérdida más grande de energía sobre la persona
gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guantelete personaje = gemaMasPoderosaRec personaje (gemas guantelete)

gemaMasPoderosaRec :: Personaje -> [Gema] -> Gema
gemaMasPoderosaRec _ [gema] = gema
gemaMasPoderosaRec personaje (gema1:gema2:gemas)
    | (energia.gema1) personaje > (energia.gema2) personaje = gemaMasPoderosaRec personaje (gema2:gemas) 
    | otherwise = gemaMasPoderosaRec personaje (gema1:gemas)



{-
Justifique si se puede ejecutar, relacionándolo con conceptos vistos en la cursada:
● gemaMasPoderosa punisher guanteleteDeLocos3 
Revienta porque la lista de gemas es infinita y Haskell no puede evaluarla. DIVERGE

● usoLasTresPrimerasGemas guanteleteDeLocos punisher
-}
infinitasGemas :: Gema -> [Gema]  -- lista infinita de una determinada gema
infinitasGemas gema = gema:infinitasGemas gema

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas elTiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) unGuantelete
