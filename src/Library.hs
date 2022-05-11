{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Library where
import PdePreludat

-- 1. Modelar a los animales: escribir un sinónimo de tipo y definir algunos ejemplos de animales
-- como constantes. De un animal se sabe su coeficiente intelectual (un número), su especie (un
-- string) y sus capacidades (strings).

data Animal = Animal {
    coeficienteIntelectual :: Number,
    especie :: String,
    capacidades :: [String]
}deriving(Show ,Eq)

-- ejemplos de animales
perro = Animal 30 "perro" ["Ladrar","Jugar"]
perroParlante = Animal 30 "perro" ["hablar","Jugar"]
tigre = Animal 61 "tigre" ["Veloz","Camuflar","hablar"]


ratonCrack = Animal 101 "raton" ["Romper las bolas"]
ratonFalladito = Animal 99 "raton" ["Fallado nomas"]

elefante = Animal 300 "elefante" ["Inteligente"]

-- 2. Transformar a un animal de laboratorio:
-- ● inteligenciaSuperior: Incrementa en n unidades su coeficiente intelectual

inteligenciaSuperior :: Number -> Animal -> Animal
inteligenciaSuperior cantidadUnidades unAnimal  = unAnimal { coeficienteIntelectual = coeficienteIntelectual unAnimal + cantidadUnidades}

-- ● pinkificar: quitarle todas las habilidades que tenía
pinkificar :: Animal -> Animal
pinkificar unAnimal = unAnimal { capacidades = []}

-- ● superpoderes:  le da habilidades nuevas
    -- ○ En caso de ser un elefante: le da la habilidad “no tenerle miedo a los ratones”
    -- ○ En caso de ser un ratón con coeficiente intelectual mayor a 100: le agrega la
    -- habilidad de “hablar”.
    -- ○ Si no, lo deja como está.

superpoderes :: Animal -> Animal
superpoderes (Animal iq "elefante" capacidades ) = Animal iq "elefante" ("no tenerle miedo a los ratones": capacidades)

superpoderes (Animal iq "raton" capacidades )
        |   siTieneIQMayorA 100 (Animal iq "raton" capacidades ) = Animal iq "raton" ("hablar": capacidades )
        |   otherwise = Animal iq "raton" capacidades


-- superpoderes ratonCrack Animal {coeficienteIntelectual = 101, especie = "raton", capacidades = ["hablar","Romper las bolas"]}
-- superpoderes ratonFalladito  Animal {coeficienteIntelectual = 99, especie = "raton", capacidades = ["Fallado nomas"]}

siTieneIQMayorA :: Number -> Animal -> Bool
siTieneIQMayorA iQ = (>iQ).coeficienteIntelectual


-- 3. Los científicos muchas veces desean saber si un animal cumple ciertas propiedades, porque
-- luego las usan como criterio de éxito de una transformación. Desarrollar los siguientes criterios:

-- ● antropomórfico: si tiene la habilidad de hablar y su coeficiente es mayor a 60

antropomórfico :: Animal -> Bool
antropomórfico unAnimal  = siTieneIQMayorA  60 unAnimal && tieneLaHabilidad "hablar" unAnimal --mejorar

tieneLaHabilidad :: String -> Animal -> Bool
tieneLaHabilidad habilidad = elem habilidad . capacidades

-- noTanCuerdo: si tiene más de dos habilidades debe hacer sonidos pinkiescos. Hacer una
-- función pinkiesco, que significa que la habilidad empieza con “hacer ”, y luego va seguido
-- de una palabra "pinkiesca", es decir, con 4 letras o menos y al menos una vocal

noTanCuerdo :: Animal -> Bool
noTanCuerdo unAnimal
       |   tieneMasDe_N_habilidades 2 unAnimal =  any pinkiesco $ capacidades unAnimal
       |   otherwise = False

tieneMasDe_N_habilidades :: Number -> Animal -> Bool
tieneMasDe_N_habilidades cantidad = (>cantidad ).length.capacidades

pinkiesco :: String -> Bool
pinkiesco palabra = empiezaConPalabraHAcer (take 6 palabra)  && palabraPinkiesca (drop 6 palabra)

empiezaConPalabraHAcer ::String -> Bool
empiezaConPalabraHAcer palabra =  palabra == "hacer "

palabraPinkiesca :: String -> Bool
palabraPinkiesca  palabra = ((<=4) . length) palabra && tieneAlMenosUnaVocal palabra

esUnaVocal :: Char -> Bool
esUnaVocal letra =  letra == 'a' || letra == 'e' || letra == 'i' || letra == 'o' || letra == 'u'

tieneAlMenosUnaVocal :: String -> Bool
tieneAlMenosUnaVocal = any esUnaVocal

-- 4. Los científicos construyen experimentos: un experimento se compone de un conjunto de
-- transformaciones sobre un animal, y un criterio de éxito. Se pide:
-- ● Modelar a los experimentos: dar un sinónimo de tipo.
-- ● Desarollar experimentoExitoso: Dado un experimento y un animal, indica si al aplicar
-- sucesivamente todas las transformaciones se cumple el criterio de éxito.

-- type TransformacionAnimal = Animal -> Animal 

-- data Experimento = Experimento {
--     transformacion :: [Animal -> Animal],
--     criterioDeExito :: Animal -> Bool,
-- }

-- unExperimento = Experimento [inteligenciaSuperior 10 ,pinkificar] 
