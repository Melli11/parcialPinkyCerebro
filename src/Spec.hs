{-# LANGUAGE BlockArguments #-}
module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
    suiteDeTestsDeParte1
    suiteDeTestsDeParte2
    -- suiteDeTestsDeParte3
suiteDeTestsDeParte1 = describe "Punto 1 : Modelando datos" $ do
    let elefante = Animal 300 "elefante" ["Inteligente"]
    describe "Modelos de datos " $ do
        it "Dado un animal de iq 100 que es un elefante y tiene como parte de sus habilidades ser Inteligente " $ do
            elefante `shouldBe` Animal 300 "elefante" ["Inteligente"]

suiteDeTestsDeParte2 = describe "Punto 2: Transformaciones" $ do 
    let perro = Animal 30 "perro" ["Ladrar","Jugar"]
    let tigre = Animal 50 "tigre" ["Veloz","Camuflar"]
    let ratonCrack = Animal 101 "raton" ["Romper las bolas"]
    let ratonFalladito = Animal 99 "raton" ["Fallado nomas"]
    describe "Inteligencia Superior" $ do
        it "Dado un animal incremento su inteligencia en n unidades" $ do
            inteligenciaSuperior 20 perro `shouldBe` Animal {coeficienteIntelectual = 50, especie = "perro", capacidades = ["Ladrar","Jugar"]}
    describe "Pinkificar" $ do
        it "Dado un animal le saco todas sus habilidades" $ do
            pinkificar perro `shouldBe` Animal {coeficienteIntelectual = 30, especie = "perro", capacidades = []}
    describe "Superpoderes" $ do
        it "Dado un animal que es un elefante le agrego la habilidad no tenerle miedo a los ratones" $ do
            superpoderes  elefante `shouldBe` Animal 300 "elefante" ["no tenerle miedo a los ratones","Inteligente"]
        it "Dado un animal que es un raton con iq MAYOR a 100 le agrego la habilidad de hablar " $ do
            superpoderes ratonCrack `shouldBe` Animal {coeficienteIntelectual = 101, especie = "raton", capacidades = ["hablar","Romper las bolas"]}
        it "Dado un animal que es un raton con iq MENOR a 100 lo dejo como está " $ do
            superpoderes ratonFalladito `shouldBe` Animal 99 "raton" ["Fallado nomas"]

-- suiteDeTestsDeParte3 = describe "Punto 3: Criterios" $ do 
--     let perroParlante = Animal 30 "perro" ["hablar","Jugar"]
--     let tigre = Animal 61 "tigre" ["Veloz","Camuflar","hablar"]
    
--     describe "Antropomorfico" $ do
--         it "Dado un animal que tiene en su reportorio de capacidades hablar, entonces es parlante " $ do
--             tieneLaHabilidad "hablar" perroParlante `shouldBe` True
--         it "Dado un animal que puede hablar pero tiene Iq menor a 60 entonces NO es antropomorfico " $ do
--             antropomórfico perroParlante `shouldBe` False
--         it "Dado un animal que puede hablar pero y Iq mayor a 60 entonces SI es antropomorfico " $ do
--             antropomórfico tigre `shouldBe` True
    


