module Lib where
import Text.Show.Functions

laVerdad = True
{- De los ratones nos interesa modelar su nombre, su edad (en años), su peso, y las enfermedades que posee.
Por ejemplo:
Cerebro es un ratón con 9 años, 0.2 kg de peso y tiene brucelosis, sarampión y tuberculosis.
Bicenterrata es un ratón con 256 años, 0.2kg de peso, y completamente sano.
Huesudo es un ratón de 4 años con 10kg de peso, y alta obesidad y sinusitis.

=================PUNTO 1=================
Modelar a los ratones mencionados.
-}
data Raton = UnRaton {
    nombre::String,
    edad::Float,
    peso::Float,
    enfermedades::[Enfermedad]
} deriving (Show,Eq)
type Enfermedad = String

cerebro :: Raton
cerebro = UnRaton{
    nombre="Cerebro",
    edad=9,
    peso=0.2,
    enfermedades=["brucelosis", "sarampion","tuberculosis"]
}

bicenterrata :: Raton
bicenterrata = UnRaton {
    nombre = "Bicenterrata",
    edad = 256,
    peso = 0.2,
    enfermedades = []
}

huesudo :: Raton
huesudo = UnRaton{
    nombre="Huesudo",
    edad=4,
    peso=10,
    enfermedades=["alta obesidad", "sinusitis"]
}


{-
=================PUNTO 2=================
Existen distintos tipos de hierbas que afectan (modifican) de diferentes maneras al ratón. Definir dichas hierbas:
-}
type Hierba = Raton -> Raton

{-
hierbaBuena, que rejuvenece al ratón a la raíz cuadrada de su edad.
Por ejemplo, si a cerebro le doy hierbaBuena, se transforma en un ratón de 3 años.
-}
hierbaBuena :: Hierba
hierbaBuena raton = cambiarEdad (sqrt (edad raton)) raton


cambiarEdad :: Float -> Raton -> Raton
cambiarEdad años raton = raton {edad=años} 



{- hierbaVerde, elimina las enfermedades que terminen de cierta forma.
Por ejemplo, si a cerebro le doy la hierbaVerde del tipo “sis”, queda sólo con sarampión.
-}

hierbaVerde :: String -> Hierba
hierbaVerde sufijo raton = raton {enfermedades = filter (not.(terminaCon sufijo)) (enfermedades raton) }

terminaCon :: String -> String -> Bool
terminaCon terminacion enfermedad = terminacion == drop (length enfermedad - length terminacion) enfermedad

{- hierbaVerde1 :: String -> Hierba
hierbaVerde1 sufijo raton = raton {enfermedades = filter (noTerminaCon sufijo) (enfermedades raton) }

noTerminaCon :: String -> String -> Bool
noTerminaCon terminacion enfermedad = terminacion /= drop (length enfermedad - length terminacion) enfermedad
-}

{- alcachofa, hace que el ratón pierda peso en un 10% si pesa más de 2kg, sino pierde un 5%.
Por ejemplo, un raton de 3 kg queda con 2,7 kg y cerebro queda con 0.19 kg. -}

alcachofa :: Hierba
alcachofa = perderPesoPorcentual 

perderPesoPorcentual :: Raton -> Raton
perderPesoPorcentual raton  | peso raton > 2.0 = perderPeso (peso raton *0.10) raton
                            | otherwise = perderPeso (peso raton *0.05) raton

perderPeso :: Float -> Raton -> Raton
perderPeso cantidad raton = raton {peso = peso raton - cantidad }  
--hierbaZort, hace que el ratón se transforme en Pinky, perdiendo todas sus enfermedades y quedando con 0 años de edad.

hierbaZort :: Hierba
hierbaZort = transformarEnPinky

transformarEnPinky :: Raton->Raton
transformarEnPinky raton = raton{
    nombre="Pinky",
    edad=0,
    enfermedades=[]
}


--hierbaDelDiablo, hace que el ratón pierda 0.1kg (sin disminuir de 0) y elimina todas las enfermedades con menos de 10 letras. 

hierbaDelDiablo :: Hierba
hierbaDelDiablo raton = ((perderPeso 0.1). sanar) raton            

sanar :: Raton -> Raton
sanar raton = raton {enfermedades = filter ((>10).length) (enfermedades raton)}

{-
=================PUNTO 3=================
Medicamentos: Los medicamentos son la administración sucesiva de un conjunto de hierbas. Se pide crear los 
siguientes medicamentos para luego poder administrarlos en un ratón: -}

{-Hacer el pondsAntiAge, que es un medicamento que está hecho con 3 hierbas buenas y una alcachofa. iterate
Por ejemplo, si se lo administramos al ratón Bicenterrata, queda con 2 años y 0.19 kg -}

type Medicamento = Hierba

pondsAntiAge :: Medicamento
pondsAntiAge = alcachofa.hierbaBuena.hierbaBuena.hierbaBuena

{- Hacer el reduceFatFast, (que viene en distintas potencias) y es un medicamento compuesto por una hierbaVerde de “obesidad” 
y tantas alcachofas como indique su potencia.
Por ejemplo administrándole a Huesudo un reduceFatFast de potencia 1 hace que huesudo pase a pesar 9 kg y sólo quede 
con sinusitis. 
Si en lugar de la 1 le administramos un reduceFatFast de potencia 2, pasa a pesar 8.1 kg y queda también solo con sinusitis.-}

reduceFatFast :: Int -> Medicamento
-- reduceFatFast potencia raton = hierbaVerde "obesidad" (potenciarAlcachofa potencia raton)
reduceFatFast potencia = (hierbaVerde "obesidad") . (potenciarAlcachofa potencia)

potenciarAlcachofa :: Int -> Hierba
potenciarAlcachofa potencia raton = foldr id raton (replicate potencia alcachofa)

{-Hacer la pdepCilina, que es un medicamento que usa hierbasVerdes para curar todas las enfermedades 
infecciosas. Las enfermedades infecciosas son aquellas cuyo nombre termina de alguna de estas formas 
(utilizar esta constante): sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]
-}

pdepCilina :: Medicamento
pdepCilina raton = foldr hierbaVerde raton sufijosInfecciosas

sufijosInfecciosas :: [String]
sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

{- 
=================PUNTO 4=================
Experimento: Los laboratorios antes de publicar un medicamento, lo prueban con distintos ratones para evaluar los resultados:
Hacer la función que encuentra la cantidadIdeal. Recibe una condición y dice cuál es el primer número natural que la cumple.
> cantidadIdeal even           > cantidadIdeal (>5)
2                              6
-}
cantidadIdeal :: (Int->Bool)->Int

cantidadIdeal condicion = head (filter condicion [1..])

{-
Saber si un medicamento lograEstabilizar una comunidad de ratones. Esto sucede cuando, luego de aplicarle el medicamento a todos 
los ratones de la comunidad, se elimina el sobrepeso y todos tienen menos de 3 enfermedades. 
Un ratón tiene sobrepeso si pesa más de 1kg.
-}

lograEstabilizar :: [Raton]->Medicamento->Bool
lograEstabilizar ratones medicamento = all cumpleCriterio (aplicarMedicamentoATodos medicamento ratones)

cumpleCriterio :: Raton->Bool
cumpleCriterio raton = (not.tieneSobrepeso) raton && length (enfermedades raton) < 3

aplicarMedicamentoATodos :: Medicamento->[Raton]->[Raton]
aplicarMedicamentoATodos medicamento ratones = map medicamento ratones

tieneSobrepeso :: Raton -> Bool
tieneSobrepeso raton = peso raton >= 1

comunidadRatones :: [Raton]
comunidadRatones = [huesudo,cerebro,bicenterrata]

{-
Diseñar el siguiente experimento: dado una comunidad de ratones, encontrar la potencia ideal del reduceFatFast 
necesaria para estabilizar la comunidad.
-}


--cantidadIdeal condicion = head (filter condicion [1..])

potenciaIdeal :: [Raton]->Int
--potenciaIdeal comunidad = cantidadIdeal (\potencia -> lograEstabilizar comunidad (reduceFatFast potencia))

potenciaIdeal ratones = cantidadIdeal (lograEstabilizarFatFast ratones)

lograEstabilizarFatFast :: [Raton] -> Int -> Bool
lograEstabilizarFatFast ratones potencia = lograEstabilizar ratones (reduceFatFast potencia)

{-
5) Queremos saber si un medicamento logra estabilizar una comunidad infinita. ¿Podemos saberlo?
 Responder en estos dos casos:
Si todos los ratones quedan con menos de 1kg y sin enfermedades. Justificar.
-}
comunidadInfinita :: Raton->[Raton]
comunidadInfinita raton = repeat raton
{-
Si un ratón queda con 2kg y 4 enfermedades. Justificar.
-}