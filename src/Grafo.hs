module Grafo (Grafo, vacio, nodos, vecinos, agNodo, sacarNodo, agEje, lineal, union', clausura) where

import Data.List

data Grafo a = G [a] (a -> [a])

instance (Show a) => Show (Grafo a) where
  show (G n e) = "[\n" ++ concat (map (\x -> " " ++ show x ++ " -> " ++ show (e x) ++ "\n") n) ++ "]"


-- ---------------------------------Sección 3--------- Grafos ---------------------------


-- Ejercicio 1
-- -------------------------------------------------------------------------- --
-- El grafo vacio se contruye usando una lista vacia de nodos y una funcion   --
-- que evaluada en cualquier nodo devuelve una lista vacia de vecinos.        --
-- Ejemplo1: vacio DEVUELVE                                                   --
-- [                                                                          --
-- ]                                                                          --
-- -------------------------------------------------------------------------- --
vacio :: Grafo a
vacio = G [] (\n -> [])

-- Ejercicio 2
-- -------------------------------------------------------------------------- --
-- La lista de nodos es obtenida directamente de la declaracion del grafo.    --
-- Ejemplo1: nodos vacio = []                                                 --
-- Ejemplo2: nodos (agNodo 1 vacio) = [1]                                     --
-- -------------------------------------------------------------------------- --
nodos :: Grafo a -> [a]
nodos (G ns r) = ns

-- Ejercicio 3
-- -------------------------------------------------------------------------- --
-- La lista de vecionos resulta de la aplicacion de la funcion de relacion    --
-- declarada en el grafo a un nodo en particular.                             --
-- Ejemplo1: vecinos vacio 5 = []                                             --
-- Ejemplo2: vecinos(agEje (1,2) (agNodo 2 (agNodo 1 vacio))) 1 = [2]         --
-- -------------------------------------------------------------------------- --
vecinos :: Grafo a -> a -> [a]
vecinos (G ns r) n = r n

-- Ejercicio 4 
-- -------------------------------------------------------------------------- --
-- Si el nodo a ser agregado ya pertenece al grafo, devuelve el mismo grafo,  --
-- evitando de esta forma nodos duplicados en la declaracion del mismo.       --
-- En caso contrario, devuelve un grafo que se conforma con la lista de nodos --
-- originales agregando el nuevo nodo y la relacion original a la cual se le  --
-- define que al ser evaluada en este nuevo nodo, retorne vacio.              --
-- Ejemplo1: agNodo 1 vacio DEVUELVE                                          --
-- [                                                                          -- 
--  1 -> []                                                                   -- 
-- ]                                                                          -- 
-- -------------------------------------------------------------------------- --
agNodo :: Eq a => a -> Grafo a -> Grafo a
agNodo n g@(G ns r) = if (n `elem` (nodos g)) then g else (G (ns ++ [n]) newr) 
  where newr n' = if (n' == n) then [] else r n' 

-- Ejercicio 5
-- -------------------------------------------------------------------------- --
-- Si el nodo a ser sacado no pertenece al grafo, devuelve el mismo grafo.    --
-- En caso contrario, devuelve un grafo que se conforma con la lista de nodos --
-- originales quitando de ella este nodo y la relacion original a la cual se  --
-- le define sacar a este nodo como vecino de cualquier valuacion.            --
-- Ejemplo1: sacarNodo 2 (agEje (1,2) (agNodo 2 (agNodo 1 vacio))) DEVUELVE   --
-- [                                                                          --
--  1 -> []                                                                   --
-- ]                                                                          --
-- -------------------------------------------------------------------------- --
sacarNodo :: Eq a => a -> Grafo a -> Grafo a
sacarNodo n g@(G ns r) = if (n `elem` (nodos g)) then (G (filter (/= n) ns) newr) else g
  where newr n' = filter (/= n) (vecinos g n')

-- Ejercicio 6
-- -------------------------------------------------------------------------- --
-- Si se verifica que algun nodo del eje a ser agregado no pertenece al grafo --
-- o el eje ya existe en el grafo, se devuelve el mismo grafo. En caso        --
-- contrario, devuelve un grafo que se conforma con la lista original de      --
-- nodos y la relacion original a la cual se le define que retorne un veciono --
-- adicional al ser evaluada en la primer componente del eje.                 --
-- Ejemplo1: agEje (1,2) vacio DEVUELVE                                       --
-- [                                                                          --
-- ]                                                                          --
-- Ejemplo2: agEje (1,1) (agNodo 1 vacio) DEVUELVE                            --
-- [                                                                          --
--  1 -> [1]                                                                  --
-- ]                                                                          --
-- -------------------------------------------------------------------------- --
agEje :: Eq a => (a,a) -> Grafo a -> Grafo a
agEje (n1, n2) g@(G ns r) = if (n1 `elem` (nodos g)) && (n2 `elem` (nodos g)) && (not (n2 `elem` (vecinos g n1))) then (G ns newr) else g
  where newr n' = if (n' == n1) then (vecinos g n1) ++ [n2] else r n' 
  
-- Ejercicio 7 
-- -------------------------------------------------------------------------- --
-- Para construir el grafo deseado, primero se crea un grafo que contiene     --
-- solo los nodos que son dados como parametro, utilizando un foldl y la      --
-- funcion agNodo. Luego se agregan los ejes a este grafo incial utilizando   --
-- un foldr y la funcion agEje, obteniendo los ejes de una lista de tuplas    --
-- que surge del emparejamiento de los nodos iniciales menos el ultimo con    --
-- los nodos originales menos el primero.                                     --
-- Ejemplo1: lineal [1..4] DEVUELVE                                           --
-- [                                                                          --
--  1 -> [2]                                                                  --
--  2 -> [3]                                                                  --
--  3 -> [4]                                                                  --
--  4 -> []                                                                   --
-- ]                                                                          --
-- -------------------------------------------------------------------------- --
lineal :: Eq a => [a] -> Grafo a
lineal ns =  foldr agEje (foldl (flip agNodo) vacio ns) (zip (init ns) (tail ns))

-- Ejercicio 8
-- -------------------------------------------------------------------------- --
-- Esta funcion fue renombrada con una prima porque utilizamos Data.List      --
-- que tiene la misma funcion declarada.                                      --
-- Para construir el grafo deseado, se agregan al segundo grafo los nodos del --
-- primero utilizando un foldr y agNodo que tiene en cuenta los repetidos.    --
-- Luego a este segundo grafo ampliado de nodos, se le agregan todos los ejes --
-- del primer grafo utilizando un foldr y agEje que tiene en cuenta los ejes  --
-- duplicados, obteniendo la lista de ejes con una lista por comprension      --
-- que recorre todos los nodos del primer grafo y por cada uno de ellos,      --
-- recorre todos sus veciones para formar las tuplas necesarias.              --
-- Ejemplo1: union' (agEje (1,2) (agNodo 2 (agNodo 1 vacio)))                 --
--                  (agNodo 3 (agEje (1,2) (agNodo 2 (agNodo 1 vacio))))      --
--                  DEVUELVE                                                  --
-- [                                                                          --
--  1 -> [2]                                                                  --
--  2 -> []                                                                   --
--  3 -> []                                                                   --
-- ]                                                                          --
-- -------------------------------------------------------------------------- --
union' :: Eq a => Grafo a -> Grafo a -> Grafo a
union' g1 g2 = foldr agEje (foldr agNodo g2 (nodos g1)) [(n1, n2) | n1 <- (nodos g1), n2 <- (vecinos g1 n1)]
  
-- Ejercicio 9 
-- -------------------------------------------------------------------------- --
-- Aplicando la funcion de punto fijo a la funcion de relacion de vecinos del --
-- grafo obtenemos la clausura del grafo.                                     --
-- La funcion de punto fijo recibe una funcion que determina el criterio de   --
-- igualdad, una funcion de composicion y un conjunto de entrada.             --
-- La idea es aplicar la composicion sobre el conjunto de entrada hasta       --
-- satisfacer el criterio de igualdad para retornar el resultado.             --
-- En este caso en particular, el conjunto de entrada es la funcion de        --
-- relacion de vecionos del grafo, expresada como lista de tuplas (ejes).     --
-- La funcion de composicion es la funcion de trancitividad sobre los ejes,   --
-- de tal forma que agrega al conjunto original todos los ejes (x,z) tal que  --
-- para todo (x,y) e (y',z) se cumple que y == y'                             --
-- De esta forma si componemos esta funcion hasta que el conjunto de ejes     --
-- no cambie, obtenemos la clausura. Para determinar cuando la ultima         --
-- aplicacion fue igual a su anterior se usa el criterio de subconjunto.      --
-- Ya que si la ultima aplicacion que agrega ejes esta incluida en la         --
-- anteior implica que ya no se pueden agregar ejes y los conjuntos son       --
-- iguales.                                                                   --
-- Para que el grafo sea tambien reflexivo se agregan estos ejes de forma     --
-- expresa.                                                                   --
-- El grafo deseado resulta de utilizar los nodos del grafo incial y generar  --
-- una nueva relacion de vecionos partiendo de la lista de ejes que devuelve  --
-- la funcion de punto fijo.                                                  --
-- Para esto, la funcion evaluada en un nodo, usa FILTER sobre la primera     --
-- componente de los ejes que da punto fijo y luego devuelve la lista de      --
-- vecions que resulta de aplicar MAP de la segunda componente de la lista    --
-- filtrada.                                                                  --
-- Ejemplo1: clausura (lineal [1..4]) DEVUELVE                                --
-- [                                                                          --
--  1 -> [2,1,3,4]                                                            --
--  2 -> [3,2,4]                                                              --
--  3 -> [4,3]                                                                --
--  4 -> [4]                                                                  --
-- ]                                                                          --
-- -------------------------------------------------------------------------- --
clausura :: Eq a => Grafo a -> Grafo a
clausura g@(G ns r) = (G ns (\n -> map (snd) (filter ((==n).fst) (puntoFijo subconjunto componer (ejesMasReflex g)))))

-- -------------------------------------------------------------------------- --
-- Aplica una funcion hasta obtener un punto fijo, verificando con una        --
-- funcion de igualdad.                                                       --
-- En este caso las aplicaciones sucesivas se logran utilizando iterate.      --
-- Se arma una lista de tuplas que contienen dos aplicaciones sucesivas de la --
-- la funcion y se filtran las que las componentes comple el criterio de      --
-- igualdad. De esta forma el primer elemento de la lista de tuplas, tiene en --
-- sus componentes el mismo punto fijo, que es el resultado.                  --
-- -------------------------------------------------------------------------- --
puntoFijo :: Eq a => (a -> a -> Bool) -> (a -> a) -> a -> a
puntoFijo fEq fOp z = let comp = iterate fOp z
                      in head [y | (x, y) <- zip comp (tail comp), fEq y x]

-- -------------------------------------------------------------------------- --
-- Convierte la funcion de relacion de veciones del grafo en un lista de ejes --
-- a los que se les agregan por conveniencia los necesarios para tener una    --
-- relacion reflexiva.                                                        --
-- -------------------------------------------------------------------------- --
ejesMasReflex :: Eq a => Grafo a -> [(a,a)]
ejesMasReflex g@(G ns r) = [(n, v) | n <- ns, v <- (r n)] `union` [(n, n) | n <- ns]

-- -------------------------------------------------------------------------- --
-- Utilizando listas por compresion determina si una es un subconjunto        --
-- -------------------------------------------------------------------------- --
subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto xs ys = and [x `elem` ys | x <- xs]

-- -------------------------------------------------------------------------- --
-- Esta es la funcion de transitividad sobre ejes. Agrega a los ejes          --
-- originales el resultado de todos los ejes (x,z) tal que  para todo (x,y) e --
-- (y',z) se cumple que y == y'                                               --
-- -------------------------------------------------------------------------- --
componer :: Eq a => [(a,a)] -> [(a,a)]
componer xs = xs `union` [(x, z) | (x, y) <- xs, (y', z) <- xs, y == y']
