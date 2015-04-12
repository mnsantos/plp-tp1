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

sacarNodoVer2 :: Eq a => a -> Grafo a -> Grafo a
sacarNodoVer2 n (G ns r) = G (sacarNodo' ns n) (\x -> case x of
								_ | n==x -> []
								  | otherwise -> filter (/=n) (vecinos (G ns r) x))

sacarNodo' :: Eq a => [a] -> (a -> [a])
sacarNodo' = foldr g (const [])
	where g x rec n = if (n==x) then (rec n) else (x:(rec n))

-- Ejercicio 6
agEje :: Eq a => (a,a) -> Grafo a -> Grafo a
agEje (n1, n2) g@(G ns r) = if (n1 `elem` (nodos g)) && (n2 `elem` (nodos g)) && (not (n2 `elem` (vecinos g n1))) then (G ns newr) else g
  where newr n' = snd (head (filter ((== n').fst) [(nodo, (vecinos g nodo)) | nodo <- nodos g, nodo /= n1] ++ [(n1, ((vecinos g n1) ++ [n2]))]))

agEjeVer2 :: Eq a => (a,a) -> Grafo a -> Grafo a
agEjeVer2 (n1,n2) g = G (nodos g) (\n -> case n of
								_ | n==n1 -> (vecinos g n)++[n2] 
								  | otherwise -> vecinos g n)
-- Ejercicio 7
lineal :: [a] -> Grafo a7
lineal = undefined

-- Ejercicio 8
union :: Grafo a -> Grafo a -> Grafo a
union g1 g2 = G (merge (nodos g1) (nodos g2)) newr
	where newr n = (vecinos g1 n) ++ (vecinos g2 n)

merge :: [a] -> [a] -> [a]
merge = foldr (\x rec ys-> x) (const [])

-- Ejercicio 9
clausura :: Grafo a -> Grafo a
clausura = undefined







