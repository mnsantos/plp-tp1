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
agNodo n g@(G ns r) = if (n `elem` (nodos g)) then g else (G (ns ++ [n]) newr) 
  where newr n' = snd (head (filter ((== n').fst) [(nodo, (vecinos g nodo)) | nodo <- nodos g] ++ [(n, [])]))

-- Ejercicio 5
sacarNodo :: Eq a => a -> Grafo a -> Grafo a
sacarNodo n g@(G ns r) = if (n `elem` (nodos g)) then (G (filter (/=n) ns) newr) else g
  where newr n' = snd (head (filter ((== n').fst) [(nodo, (vecinos g nodo)) | nodo <- nodos g, nodo /= n]))

-- Ejercicio 6
agEje :: Eq a => (a,a) -> Grafo a -> Grafo a
agEje (n1, n2) g@(G ns r) = if (n1 `elem` (nodos g)) && (n2 `elem` (nodos g)) && (not (n2 `elem` (vecinos g n1))) then (G ns newr) else g
  where newr n' = snd (head (filter ((== n').fst) [(nodo, (vecinos g nodo)) | nodo <- nodos g, nodo /= n1] ++ [(n1, ((vecinos g n1) ++ [n2]))]))

-- Ejercicio 7
lineal :: [a] -> Grafo a
lineal ns =  foldr agEje (foldl (flip agNodo) vacio ns) [(n1, n2) | n1 <- (init ns), n2 <- (tail ns)]

-- Ejercicio 8
union :: Grafo a -> Grafo a -> Grafo a
union = undefined

-- Ejercicio 9
clausura :: Grafo a -> Grafo a
clausura = undefined





