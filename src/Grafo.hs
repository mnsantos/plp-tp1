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
--vecinos toma un grafo G y un nodo N y aplica la función r a N.
vecinos :: Grafo a -> a -> [a]
vecinos (G ns r) n = r n

-- Ejercicio 4 
--agNodo toma un nodo N y un grafo G y retorna un nuevo grafo G' que agrega N a la lista de nodos de G 
--siempre y cuando no se encuentre presente en la misma. Además, construye G' con una nueva función r(newr)
--de manera que si se evalua en N retorna la lista vacía.
agNodo :: Eq a => a -> Grafo a -> Grafo a
agNodo n g@(G ns r) = if (n `elem` (nodos g)) then g else (G (ns ++ [n]) newr) 
  where newr n' = if (n' == n) then [] else r n' 

-- Ejercicio 5
--sacarNodo toma un nodo N y un grafo G y retorna un nuevo grafo G' que filtra la lista de nodos 
--de G sacando todos los que son iguales a N. Además, construye G' con una nueva función r(newr) que filtra
--todas las ocurrencias de N en los vecinos de los nodos de G.
sacarNodo :: Eq a => a -> Grafo a -> Grafo a
sacarNodo n g@(G ns r) = if (n `elem` (nodos g)) then (G (filter (/= n) ns) newr) else g
  where newr n' = filter (/= n) (vecinos g n')

-- Ejercicio 6
--agEje toma una tupla (a1,a2) y un grafo G y retorna un nuevo grafo G' que agrega a2 a la lista de vecinos de
--a1. Para ello verifica que a1 y a2 pertenezcan a la lista de nodos de G y que, además, a2 no pertenezca
--a la lista de nodos de a1.
agEje :: Eq a => (a,a) -> Grafo a -> Grafo a
agEje (n1, n2) g@(G ns r) = if (n1 `elem` (nodos g)) && (n2 `elem` (nodos g)) && (not (n2 `elem` (vecinos g n1))) then (G ns newr) else g
  where newr n' = if (n' == n1) then (vecinos g n1) ++ [n2] else r n' 
  
-- Ejercicio 7
--lineal toma una lista L de nodos y contruye un grafo lineal con dichos elementos. Para implementarlo, usamos la función
--foldr que utiliza agEje como función combinadora y que tiene como caso base el grafo G con los nodos de L. Foldr se aplica a
--a una lista de tuplas (a1,a2) que se contruye de la siguiente manera:
--Si la lista L es [1,2,3] entonces la lista de tuplas es [(1,2),(2,3)].
lineal :: Eq a => [a] -> Grafo a
lineal ns =  foldr agEje (foldl (flip agNodo) vacio ns) (zip (init ns) (tail ns))

-- Ejercicio 8
union :: Eq a => Grafo a -> Grafo a -> Grafo a
union g1 g2 = foldr agEje (foldr agNodo g2 (nodos g1)) [(n1, n2) | n1 <- (nodos g1), n2 <- (vecinos g1 n1)]
  
-- Ejercicio 9
clausura :: Grafo a -> Grafo a
clausura = undefined
--clausura = puntoFijo clausurar clausuraReflexiva

clausuraReflexiva :: Eq a => Grafo a -> Grafo a
clausuraReflexiva g@(G ns _) = foldr agEje g (zip ns ns) 

puntoFijo :: Eq a => (a->a) -> (a->a)
puntoFijo f x = head [l!!i | i<-[0..], l!!i==l!!(i+1)]
	where l = iterate f x
