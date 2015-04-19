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
visibilidad = foldExp (const 0) id max max (+1) (+1)

-- Ejercicio 12
extraer :: Exp -> [Prop]
extraer = foldExp (:[]) id (++) (++) id id

-- Ejercicio 13
eval :: Modelo -> Mundo -> Exp -> Bool
eval = undefined

eval' :: Modelo -> Exp -> Mundo -> Bool
eval' modelo@(K g r) e mundo = foldExp vale not || && (\x -> foldr (eval' modelo x) [] (vecinos g mundo)) () 
	where vale = (\x -> mundo 'elem' (r x))
	--to DO

-- Ejercicio 14
valeEn :: Exp -> Modelo -> [Mundo]
valeEn e model@(K g r) = foldr (\x rec -> if (eval model e x) then (:) else rec) [] (nodos g))
--to TEST

-- Ejercicio 15
quitar :: Exp -> Modelo -> Modelo
quitar e model@(K g r) = foldr (\x rec -> if not(eval model e x) then (sacarNodo x g) else rec) model (nodos g))
--to TEST

-- Ejercicio 16
cierto :: Modelo -> Exp -> Bool
cierto model@(K g r) e = foldr (\x rec -> if (eval model e x) then and else 

cierto model@(K g r) e = foldr (\x rec -> eval model e x && rec) True (nodos g)