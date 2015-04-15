module Lomoba where
import Grafo
import Tipos


-- ---------------------------------Sección 6--------- Lomoba ---------------------------

-- Ejercicio 10
foldExp :: (Prop -> a) -> (a -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> (a -> a) -> Exp -> a
foldExp fVarP fNot fOr fAnd fD fB e = 
  let rec = foldExp fVarP fNot fOr fAnd fD fB 
  in case e of  Var p -> fVarP p
                Not e1 -> fNot (rec e1)
                Or e1 e2 -> fOr (rec e1) (rec e2)
                And e1 e2 -> fAnd (rec e1) (rec e2)
                D e1 -> fD (rec e1)
                B e1 -> fB (rec e1)
     
-- Ejercicio 11
visibilidad :: Exp -> Integer
visibilidad = undefined

-- Ejercicio 12
extraer :: Exp -> [Prop]
extraer = foldExp (\x -> [x]) id (++) (++) id id

-- Ejercicio 13
eval :: Modelo -> Mundo -> Exp -> Bool
eval = undefined

-- Ejercicio 14
valeEn :: Exp -> Modelo -> [Mundo]
valeEn = undefined

-- Ejercicio 15
quitar :: Exp -> Modelo -> Modelo
quitar = undefined

-- Ejercicio 16
cierto :: Modelo -> Exp -> Bool
cierto = undefined

