{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
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
ratonFalladito = Animal 99 "raton" ["Fallado nomas","hablar"]

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


superpoderes (Animal iq especie capacidades ) = Animal iq especie capacidades
-- superpoderes ratonCrack Animal {coeficienteIntelectual = 101, especie = "raton", capacidades = ["hablar","Romper las bolas"]}
-- superpoderes ratonFalladito  Animal {coeficienteIntelectual = 99, especie = "raton", capacidades = ["Fallado nomas"]}

siTieneIQMayorA :: Number -> Animal -> Bool
siTieneIQMayorA iQ = (>iQ).coeficienteIntelectual


-- 3. Los científicos muchas veces desean saber si un animal cumple ciertas propiedades, porque
-- luego las usan como criterio de éxito de una transformación. Desarrollar los siguientes criterios:

-- ● antropomórfico: si tiene la habilidad de hablar y su coeficiente es mayor a 60

antropomórfico :: Animal -> Bool
antropomórfico unAnimal  = siTieneIQMayorA  60 unAnimal && tieneLaHabilidad "hablar" unAnimal --Se puede mejorar???

tieneLaHabilidad :: String -> Animal -> Bool
tieneLaHabilidad habilidad = elem habilidad . capacidades

-- noTanCuerdo: si tiene más de dos habilidades puede hacer sonidos pinkiescos. Hacer una
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
palabraPinkiesca  palabra = ((<=4) . length) palabra && tieneAlMenosUnaVocal palabra --Se puede mejorar???

esUnaVocal :: Char -> Bool
esUnaVocal letra =  letra == 'a' || letra == 'e' || letra == 'i' || letra == 'o' || letra == 'u'

tieneAlMenosUnaVocal :: String -> Bool
tieneAlMenosUnaVocal = any esUnaVocal

-- 4. Los científicos construyen experimentos: un experimento se compone de un conjunto de
-- transformaciones sobre un animal, y un criterio de éxito. Se pide:
-- ● Modelar a los experimentos: dar un sinónimo de tipo.
-- ● Desarollar experimentoExitoso: Dado un experimento y un animal, indica si al aplicar
-- sucesivamente todas las transformaciones se cumple el criterio de éxito.

type Transformar_Animal = Animal -> Animal

data Experimento = Experimento {
    transformaciones :: [Transformar_Animal],
    criterioDeExito :: Animal -> Bool
}

experimentoExitoso :: Animal -> Experimento -> Bool
experimentoExitoso unAnimal unExperimento = criterioDeExito unExperimento $ aplicarUnExperimento unAnimal unExperimento

aplicarUnExperimento :: Animal -> Experimento -> Animal --un experimento es un conjunto de transformaciones
aplicarUnExperimento unAnimal unExperimento = foldl aplicarUnaTransformacion unAnimal (transformaciones unExperimento)

aplicarUnaTransformacion :: Animal -> Transformar_Animal -> Animal
aplicarUnaTransformacion unAnimal transformacion = transformacion  unAnimal


--5. Periódicamente, ACME pide informes sobre los experimentos realizados. Desarrollar los
-- siguientes reportes, que a partir de una lista de animales, una lista de capacidades y un
-- experimento (o una serie de transformaciones) permitan obtener:

-- data Animal = Animal {
--     coeficienteIntelectual :: Number,
--     especie :: String,
--     capacidades :: [String]
-- }deriving(Show ,Eq)

unExperimento :: Experimento
unExperimento = Experimento [pinkificar,inteligenciaSuperior 10,superpoderes] antropomórfico

ratonExperimental :: Animal
ratonExperimental = Animal 17 "raton" ["destruir el mundo","hacer planes malvados"]

listadoDeAnimales :: [Animal]
listadoDeAnimales = [perro,perroParlante,ratonExperimental,ratonCrack]

listadoDeCapacidades :: [String]
listadoDeCapacidades = ["nadar", "gritar","no hacer nada"] --no deberia retornar ningun iq

listadoDeCapacidades2 :: [String]
listadoDeCapacidades2 = ["no tenerle miedo a los elefantes","hablar","nadar"]


-- perro = Animal 30 "perro" ["Ladrar","Jugar"]
-- perroParlante = Animal 30 "perro" ["hablar","Jugar"]
-- tigre = Animal 61 "tigre" ["Veloz","Camuflar","hablar"]
-- ratonCrack = Animal 101 "raton" ["Romper las bolas"]
-- ratonFalladito = Animal 99 "raton" ["Fallado nomas"]
-- elefante = Animal 300 "elefante" ["Inteligente"]


-- 1. una lista con los coeficientes intelectuales de los animales que entre sus capacidades,
-- luego de efectuar el experimento, tengan ALGUNA de las capacidades dadas.

-- 1.aplicar un experimento a cada animal de la lista -> retornar una lista de animales con los experimentos aplicados
-- 2. machear la lista de strings con la lista de capacidades del punto1.
-- 3. mapear el iq 

-- TRABADO MAAL

-- 1ra idea para evitar la repeticion de logica (Primero resolver)
-- generarReporteDe:: (Ord a => Animal -> a) -> [Animal] -> [String] -> Experimento
-- generarReporteDe atributoAnimal animales listaDeCapacidades experimento = 

-- -- CONSEJO DE JUAN
-- Consejo: empeza de a 1 y después mete listas si se complica encarar todo junto
-- Por ej: una lista de animales, un experimento y una capacidad
-- Extraes la parte que usas para filtrar los animales
-- Y después modificas eso para que funcione con muchas capacidades

-- funcionAux :: [Animal] -> Experimento -> String -> Animal



-- funcionAux animales exp unaCapacidad =   filter(unaCapacidad ==) $ map (capacidades.flip aplicarUnaTransformacion exp) animales

-- aplicarUnExperimento :: Animal -> Experimento -> Animal --un experimento es un conjunto de transformaciones
-- aplicarUnExperimento unAnimal unExperimento = foldl aplicarUnaTransformacion unAnimal (transformaciones unExperimento)


-- Resolviendo a lo primate
-- generarReporteDe  animales listaDeCapacidades experimento =    map (capacidades. flip aplicarConjuntoDeExperimentos experimento) animales
    
aux animales unaCapacidad experimento = map (capacidades.flip aplicarUnExperimento experimento) animales


-- aux animales unaCapacidad experimento = map (capacidades.flip aplicarUnExperimento experimento) animales
-- [["Ladrar","Jugar"],["hablar","Jugar"],["destruir el mundo","hacer planes malvados"],["hablar","Romper las bolas"]]


otroExperimento :: Experimento
otroExperimento = Experimento [inteligenciaSuperior 10,inteligenciaSuperior 10,superpoderes] antropomórfico

--    tieneAlgunaHabilidadDeLaLista ListaDeHabilidades listadoDeAnimales = filter (tieneLaHabilidad)

-- generarPrimerReporte = filter.any 
-- 2. una lista con las especie de los animales que, luego de efectuar el experimento, tengan
-- entre sus capacidades todas las capacidades dadas.




-- aplicarConjuntoDeExperimentos :: Animal -> Experimento -> Animal
-- map (coeficienteIntelectual) . filter () . 
--  Construcciones utiles
-- map (capacidades) listaDeAnimales 
-- >> [["Ladrar","Jugar"],["hablar","Jugar"],["destruir el mundo","hacer planes malvados"],["Romper las bolas"]]
-- 

-- map coeficienteIntelectual listaDeAnimales
-- >> [30,30,17,101]



-- 3. una lista con la cantidad de capacidades de todos los animales que, luego de efectuar el
-- experimento, no tengan ninguna de las capacidades dadas.