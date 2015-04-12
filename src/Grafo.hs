module Grafo (Grafo, vacio, nodos, vecinos, agNodo, sacarNodo, agEje, lineal, union, clausura) where

data Grafo a = G [a] (a -> [a])

instance (Show a) => Show (Grafo a) where
	show (G n e) = "[\n" ++ concat (map (\x -> " " ++ show x ++ " -> " ++ show (e x) ++ "\n") n) ++ "]"


-- ---------------------------------Sección 3--------- Grafos ---------------------------

-- Ejercicio 1
vacio :: Grafo a
vacio = G [] (\n -> [])

-- Ejercicio 2
nodos :: Grafo a -> [a]
nodos (G ns r) = ns

-- Ejercicio 3
vecinos :: Grafo a -> a -> [a]
vecinos (G ns r) n = r n

-- Ejercicio 4 
agNodo :: Eq a => a -> Grafo a -> Grafo a
agNodo n (G ns r) = if (n `elem` ns) then (G ns r) else (G (ns ++ [n]) r) 

-- Ejercicio 5
sacarNodoVer2 :: Eq a => a -> Grafo a -> Grafo a
sacarNodoVer2 n (G ns r) = G (sacarNodo' ns n) (\x -> case x of
								_ | n==x -> []
								  | otherwise -> filter (/=n) (vecinos (G ns r) x))

sacarNodo' :: Eq a => [a] -> (a -> [a])
sacarNodo' = foldr g (const [])
	where g x rec n = if (n==x) then (rec n) else (x:(rec n))

-- Ejercicio 6
agEjeVer2 :: Eq a => (a,a) -> Grafo a -> Grafo a
agEjeVer2 (n1,n2) g = G (nodos g) (\n -> case n of
								_ | n==n1 -> (vecinos g n)++[n2] 
								  | otherwise -> vecinos g n)
-- Ejercicio 7
lineal :: [a] -> Grafo a7
lineal = undefined

-- Ejercicio 8
union :: Grafo a -> Grafo a -> Grafo a
union = undefined

-- Ejercicio 9
clausura :: Grafo a -> Grafo a
clausura = undefined







