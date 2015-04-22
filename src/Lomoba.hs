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
extraer = foldExp (:[]) id unionSR unionSR id id
  where unionSR xs ys = xs ++ [y | y <- ys, not (y `elem` xs)]

-- Ejercicio 13
eval :: Modelo -> Mundo -> Exp -> Bool
eval modelo mundo e = eval' modelo e mundo

eval' :: Modelo -> Exp -> Mundo -> Bool
eval' (K g fv) = foldExp fVarP fNot fOr fAnd fD fB
  where fVarP = (\p -> \mundo -> mundo `elem` (fv p))
        fNot = (\rec -> \mundo -> not (rec mundo))
        fOr = (\rec1 rec2 -> \mundo -> (rec1 mundo) || (rec2 mundo))
        fAnd = (\rec1 rec2 -> \mundo -> (rec1 mundo) && (rec2 mundo))
        fD = (\rec -> \mundo -> or [rec m | m <- (vecinos g mundo)])
        fB = (\rec -> \mundo -> and [rec m | m <- (vecinos g mundo)])
        
-- Ejercicio 14
valeEn :: Exp -> Modelo -> [Mundo]
valeEn e model@(K g r) = foldr (\mundo rec -> if (eval model mundo e) then (mundo:rec) else rec) [] (nodos g)

-- Ejercicio 15
quitar :: Exp -> Modelo -> Modelo
quitar e model@(K g r) = (K (foldr (\nodo rec -> sacarNodo nodo rec ) g (valeEn (Not e) model)) r)

-- Ejercicio 16
cierto :: Modelo -> Exp -> Bool
cierto model@(K g r) e = foldr (\mundo rec -> (eval model mundo e) && rec) True (nodos g)
