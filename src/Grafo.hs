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
sacarNodo :: a -> Grafo a -> Grafo a
sacarNodo = undefined

-- Ejercicio 6
agEje :: (a,a) -> Grafo a -> Grafo a
agEje = undefined

-- Ejercicio 7
lineal :: [a] -> Grafo a
lineal = undefined

-- Ejercicio 8
union :: Grafo a -> Grafo a -> Grafo a
union = undefined

-- Ejercicio 9
clausura :: Grafo a -> Grafo a
clausura = undefined





