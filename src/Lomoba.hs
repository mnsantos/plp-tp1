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
eval (K g fv) m = foldExp fVarP fNot fOr fAnd fD fB
  where fVarP p = m `elem`(fv p)
        fNot e1 = not (e1)
        fOr e1 e2 = e1 || e2
        fAnd e1 e2 = e1 && e2
        fD e1 = True -- fD e1 = or [(eval (K g fv) m' e1) | m' <- (vecinos g m)]
        fB e1 = True -- fB e1 = and [(eval (K g fv) m' e1) | m' <- (vecinos g m)]
        
-- Ejercicio 14
valeEn :: Exp -> Modelo -> [Mundo]
valeEn = undefined

-- Ejercicio 15
quitar :: Exp -> Modelo -> Modelo
quitar = undefined

-- Ejercicio 16
cierto :: Modelo -> Exp -> Bool
cierto = undefined

