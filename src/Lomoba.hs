module Lomoba where
import Grafo
import Tipos


-- ---------------------------------Sección 6--------- Lomoba ---------------------------

-- Ejercicio 10
foldExp :: (Prop -> a) -> (a -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> Exp -> a) -> (a -> Exp -> a) -> Exp -> a
foldExp fVarP fNot fOr fAnd fD fB e = 
  let rec = foldExp fVarP fNot fOr fAnd fD fB 
  in case e of  Var p -> fVarP p
                Not e1 -> fNot (rec e1)
                Or e1 e2 -> fOr (rec e1) (rec e2)
                And e1 e2 -> fAnd (rec e1) (rec e2)
                D e1 -> fD (rec e1) e1
                B e1 -> fB (rec e1) e1
     
-- Ejercicio 11
visibilidad :: Exp -> Integer
visibilidad = foldExp (const 0) id max max masUno masUno
  where masUno = flip (const (+1))

-- Ejercicio 12
extraer :: Exp -> [Prop]
extraer = foldExp (:[]) id unionSR unionSR loMismo loMismo
  where unionSR xs ys = xs ++ [y | y <- ys, not (y `elem` xs)]
        loMismo = flip (const id)

-- Ejercicio 13
eval :: Modelo -> Mundo -> Exp -> Bool
eval (K g fv) m = foldExp fVarP fNot fOr fAnd fD fB
  where fVarP p = m `elem`(fv p)
        fNot e1 = not (e1)
        fOr e1 e2 = e1 || e2
        fAnd e1 e2 = e1 && e2
        fD e1r e1 = or [(eval (K g fv) m' e1) | m' <- (vecinos g m)]
        fB e1r e1 = and [(eval (K g fv) m' e1) | m' <- (vecinos g m)]
        
-- Ejercicio 14
valeEn :: Exp -> Modelo -> [Mundo]
valeEn e model@(K g r) = foldr (\mundo rec -> if (eval model mundo e) then (mundo:rec) else rec) [] (nodos g)

-- Ejercicio 15
quitar :: Exp -> Modelo -> Modelo
quitar e model@(K g r) = (K (foldr (\nodo rec -> sacarNodo nodo rec ) g (valeEn (Not e) model)) r)
quitar2 e model@(K g r) = foldr (\x rec -> if (eval model x e) then rec else (K (sacarNodo x g) r)) model (nodos g)
-- quitar2 funciona mal, quitarla.

-- Ejercicio 16
cierto :: Modelo -> Exp -> Bool
cierto model@(K g r) e = foldr (\mundo rec -> (eval model mundo e) && rec) True (nodos g)
