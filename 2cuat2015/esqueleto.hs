module Routes where

import Data.List
import Data.Maybe

data PathPattern = Literal String | Capture String deriving (Eq, Show)

data Routes f = Route [PathPattern] f | Scope [PathPattern] (Routes f) | Many [Routes f] deriving Show

-- Ejercicio 1: Dado un elemento separador y una lista, se deber a partir la lista en sublistas de acuerdo a la aparicíon del separador (sin incluirlo).

split :: Eq a => a -> [a] -> [[a]]
split d = foldr f [[]] 
          where f c l@(x:xs) | c == d = []:l
                             | otherwise = (c:x):xs

-- Ejercicio 2: A partir de una cadena que denota un patrón de URL se deberá construir la secuencia de literales y capturas correspondiente.
pattern :: String -> [PathPattern]
pattern s = foldr f [] (split '/' s)
            where f c l | c == [] = l
                        | head c == ':' = Capture (tail c):l
                        | otherwise = Literal c:l

-- Ejercicio 3: Obtiene el valor registrado en una captura determinada. Se puede suponer que la captura está definida en el contexto.
type PathContext = [(String, String)]

get :: String -> PathContext -> String
get s p = snd (head (filter f p))
          where f tupla = (fst tupla) == s

-- Ejercicio 4: Dadas una ruta particionada y un patrón de URL, trata de aplicar el patrón a la ruta y devuelve, en caso de que
--              la ruta sea un prefijo válido para el patrón, el resto de la ruta que no se haya llegado a consumir y el contexto capturado hasta el punto alcanzado.
-- Se puede usar recursión explícita.

matches :: [String] -> [PathPattern] -> Maybe ([String], PathContext)
matches s pp = matches2 s pp []

matches2 :: [String] -> [PathPattern] -> PathContext -> Maybe ([String], PathContext)
matches2 s [] pc = Just (s, pc)
matches2 [] (_:_) _ = Nothing
matches2 (s:ss) (Literal pp:pps) pc | s == pp   = matches2 ss pps pc
                                    | otherwise = Nothing
matches2 (s:ss) (Capture pp:pps) pc = matches2 ss pps ((pp, s):pc)


-- DSL para rutas
route :: String -> a -> Routes a
route s f = Route (pattern s) f

scope :: String -> Routes a -> Routes a
scope s r = Scope (pattern s) r

many :: [Routes a] -> Routes a
many l = Many l

-- Ejercicio 5: Definir el fold para el tipo Routes f y su tipo. Se puede usar recursión explícita.
foldRoutes :: ([PathPattern] -> f -> b) -> ([PathPattern] -> b -> b) -> ([b] -> b) -> Routes f -> b
foldRoutes fRoute fScope fMany rutas =
	let rec = foldRoutes fRoute fScope fMany
	in case rutas of Route pp f  -> fRoute pp f
	                 Scope pp rf -> fScope pp (rec rf)
	                 Many  rfs   -> fMany (map rec rfs)

                   
-- Auxiliar para mostrar patrones. Es la inversa de pattern.
patternShow :: [PathPattern] -> String
patternShow ps = concat $ intersperse "/" ((map (\p -> case p of
  Literal s -> s
  Capture s -> (':':s)
  )) ps)

-- Ejercicio 6: Genera todos los posibles paths para una ruta definida.
paths :: Routes a -> [String]
paths = undefined

-- Ejercicio 7: Evalúa un path con una definición de ruta y, en caso de haber coincidencia, obtiene el handler correspondiente 
--              y el contexto de capturas obtenido.
{-
Nota: la siguiente función viene definida en el módulo Data.Maybe.
 (=<<) :: (a->Maybe b)->Maybe a->Maybe b
 f =<< m = case m of Nothing -> Nothing; Just x -> f x
-}
eval :: Routes a -> String -> Maybe (a, PathContext)
eval = undefined


-- Ejercicio 8: Similar a eval, pero aquí se espera que el handler sea una función que recibe como entrada el contexto 
--              con las capturas, por lo que se devolverá el resultado de su aplicación, en caso de haber coincidencia.
exec :: Routes (PathContext -> a) -> String -> Maybe a
exec routes path = undefined

-- Ejercicio 9: Permite aplicar una funci ́on sobre el handler de una ruta. Esto, por ejemplo, podría permitir la ejecución 
--              concatenada de dos o más handlers.
wrap :: (a -> b) -> Routes a -> Routes b
wrap f = undefined

-- Ejercicio 10: Genera un Routes que captura todas las rutas, de cualquier longitud. A todos los patrones devuelven el mismo valor. 
--               Las capturas usadas en los patrones se deberán llamar p0, p1, etc. 
--               En este punto se permite recursión explícita.
catch_all :: a -> Routes a
catch_all h = undefined
