module Routes where

import Data.List
import Data.Maybe

data PathPattern = Literal String | Capture String deriving (Eq, Show)

data Routes f = Route [PathPattern] f | Scope [PathPattern] (Routes f) | Many [Routes f] deriving Show

-- Ejercicio 1: Dado un elemento separador y una lista, se deber a partir la lista en sublistas de acuerdo a la aparicíon del separador (sin incluirlo).
-- Ejercicio 1
-- -------------------------------------------------------------------------- --
-- Utiliza foldr para hacer recursion sobre el string de forma tal que frente --
-- a un delimitador se cree una lista nueva                                   --
-- -------------------------------------------------------------------------- --
                                                                           
split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = foldr f [[]] s
          where f c l@(x:xs) | c == d = []:l
                             | otherwise = (c:x):xs

-- Ejercicio 2: A partir de una cadena que denota un patrón de URL se deberá construir la secuencia de literales y capturas correspondiente.
-- Ejercicio 2
-- -------------------------------------------------------------------------- --
-- Al igual que en el ejercicio anterior, utiliza foldr para hacer recursion. --
-- En este caso recursiona sobre el string spliteado.                         --
-- -------------------------------------------------------------------------- --

pattern :: String -> [PathPattern]
pattern s = foldr f [] (split '/' s)
            where f c l | c == [] = l
                        | head c == ':' = Capture (tail c):l
                        | otherwise = Literal c:l

-- Ejercicio 3: Obtiene el valor registrado en una captura determinada. Se puede suponer que la captura está definida en el contexto.
-- Ejercicio 3
-- -------------------------------------------------------------------------- --
-- Utiliza filter para filtrar las tuplas cuyo primer elemento sea el         --
-- parametro. Luego toma la cabeza de la lista filtrada y devuelve el segundo --
-- elemento de la tupla. En caso de haber multiples ocurrencias del parametro --
-- solo se devuelve el valor de la primer tupla encontrada                    --
-- -------------------------------------------------------------------------- --

type PathContext = [(String, String)]

get :: String -> PathContext -> String
get s p = snd (head (filter f p))
          where f tupla = (fst tupla) == s

-- Ejercicio 4: Dadas una ruta particionada y un patrón de URL, trata de aplicar el patrón a la ruta y devuelve, en caso de que
--              la ruta sea un prefijo válido para el patrón, el resto de la ruta que no se haya llegado a consumir y el contexto capturado hasta el punto alcanzado.
-- Se puede usar recursión explícita.
-- Ejercicio 4
-- -------------------------------------------------------------------------- --
-- Llama a una funcion auxiliar para parametrizar el PathContext.             --
-- Ante un string vacio con PathPattern restante, o un literal que no matchee --
-- se devuelve Nothing.
-- -------------------------------------------------------------------------- --

matches :: [String] -> [PathPattern] -> Maybe ([String], PathContext)
matches s [] = Just (s,[])
matches s pp = matches2 s pp []

matches2 :: [String] -> [PathPattern] -> PathContext -> Maybe ([String], PathContext)
matches2 [] (_:_) _ = Nothing
matches2 s [] pc = Just (s, pc)
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
-- Ejercicio 5
-- -------------------------------------------------------------------------- --
-- Se aplica recursivamente foldRoutes sobre la Route del Scope               -–
-- y se mapea sobre cada una de las contenidas en Many                        --
-- -------------------------------------------------------------------------- --
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
-- Ejercicio 6
-- -------------------------------------------------------------------------- --
-- Utiliza foldRoutes para recursionar y patternShow para                     -- 
-- traducir a String las rutas.                                               --
-- Es preciso diferenciar en la recursión cuando quedan                       --
-- strings vacios para no appendear una ‘/’ al final.                         --
-- -------------------------------------------------------------------------- --
paths :: Routes a -> [String]
paths rutas = foldRoutes fRoute fScope fMany rutas
              where fRoute pp f = [patternShow pp]
                    fScope pp rf = map (\r -> if null(r) then (patternShow pp) else (patternShow pp) ++ "/" ++ r) rf
                    fMany rfs = concat rfs
                  
-- Ejercicio 7: Evalúa un path con una definición de ruta y, en caso de haber coincidencia, 
--              obtiene el handler correspondiente y el contexto de capturas obtenido.
{-
Nota: la siguiente función viene definida en el módulo Data.Maybe.
 (=<<) :: (a->Maybe b)->Maybe a->Maybe b
 f =<< m = case m of Nothing -> Nothing; Just x -> f x
-}
-- Ejercicio 7
-- -------------------------------------------------------------------------- --
-- Eval hace uso de una funcion auxiliar eval' que utiliza foldRoutes para    --
-- recursionar sobre las rutas. Cabe destacar que fRoute, fScope y fMany      --
-- devuelven una funcion [String] -> Maybe (a, PathContext)                   --
--                                                                            --
-- -fRoute: aplica matches sobre el string que toma como parametro y verifica --
-- si el resultado de esta operacion es un string vacio (completamente        --
-- consumido). Si esto ocurre nos encontramos en un caso en donde no hay mas  --
-- ruta por consumir con lo que cual coincidencia. Se retorna el handler y el --
-- contexto consumido.                                                        --
-- En caso contrario se retorna Nothing.                                      --
--                                                                            --
-- -fScope: concatena el contexto consumido que obtiene de hacer matches      --
-- sobre el string que toma como parametro con el contexto consumido          --
-- de aplicar el resto de la ruta no consumida al resultado recursivo.        --                                                
--                                                                            --
-- -fMany: recorre las funciones obtenidas recursivamente aplicandoles el     --
-- string que toma como parametro. Si alguna de las funciones retorna Just    --
-- entonces hubo coincidencia y se retrona el resultado.                      --
-- -------------------------------------------------------------------------- --

eval :: Routes a -> String -> Maybe (a, PathContext)
eval rutas s = eval' rutas (split '/' s)

eval' :: Routes a -> [String] -> Maybe (a, PathContext)
eval' rutas = foldRoutes fRoute fScope fMany rutas
               where fRoute pp f = (\s -> (\(a,b) -> if (null a) then Just(f, b) else Nothing) =<< (matches s pp))
                     fScope pp rf = (\s -> (\(a,b)-> (\(a',b') -> Just(a',b++b')) =<< (rf a)) =<< (matches s pp))
                     fMany rfs = (\s -> foldr (\rf rec -> if isJust(rf s) then (rf s) else rec) Nothing rfs)         
                     
rutasFacultad = many [
  route ""             "ver inicio",
  route "ayuda"        "ver ayuda",
  scope "materia/:nombre/alu/:lu" $ many [
    route "inscribir"   "inscribe alumno",
    route "aprobar"     "aprueba alumno"
  ],
  route "alu/:lu/aprobadas"  "ver materias aprobadas por alumno"
  ]                 

-- Ejercicio 8: Similar a eval, pero aquí se espera que el handler sea una función que recibe como entrada el contexto 
--              con las capturas, por lo que se devolverá el resultado de su aplicación, en caso de haber coincidencia.
-- Ejercicio 8
-- -------------------------------------------------------------------------- --
-- Exec utiliza eval para obtener el handler y el contexto de capturas. Luego,--
-- aplica el handler al contexto.                                             --
-- -------------------------------------------------------------------------- --
exec :: Routes (PathContext -> a) -> String -> Maybe a
exec routes path = (\(f,pc) -> Just (f pc)) =<< (eval routes path)


-- Ejercicio 9: Permite aplicar una funci ́on sobre el handler de una ruta. Esto, por ejemplo, podría permitir la ejecución 
--              concatenada de dos o más handlers.
-- Ejercicio 9
-- -------------------------------------------------------------------------- --
-- Wrap utiliza foldRoutes para recursionar sobre el parametro Routes.        --
-- Unicamente altera los handlers, por lo que los paths de la ruta parametro  --
-- no se ven modificados.                                                     --
-- -------------------------------------------------------------------------- --
wrap :: (a -> b) -> Routes a -> Routes b
wrap f = foldRoutes fRoute fScope fMany
               where fRoute pp prevf = Route pp (f prevf)
                     fScope pp rf = Scope pp rf
                     fMany rfs = many rfs

-- Ejercicio 10: Genera un Routes que captura todas las rutas, de cualquier longitud. A todos los patrones devuelven el mismo valor. 
--               Las capturas usadas en los patrones se deberán llamar p0, p1, etc. 
--               En este punto se permite recursión explícita.
-- Ejercicio 10
-- -------------------------------------------------------------------------- --
-- Catch all utiliza recursion explicita en la llamada a la funcion auxiliar. --
-- Inicia un contador en 0 para generar las infinitas capturas.               --
-- -------------------------------------------------------------------------- --
catch_all :: a -> Routes a
catch_all f = catch_all_aux f 0

catch_all_aux :: a -> Int -> Routes a
catch_all_aux f i = many [ route "" f, scope (":p" ++ show i) (catch_all_aux f (i+1)) ]