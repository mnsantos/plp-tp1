module Lomoba where
import Grafo
import Tipos


-- ---------------------------------Sección 6--------- Lomoba ---------------------------
        
        
-- Ejercicio 10
-- -------------------------------------------------------------------------- --
-- Por cada constructor de Exp aplica una funcion especifica al resultado     --
-- recursivo de la expresion sin el constructor.                              --
-- -------------------------------------------------------------------------- --
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
-- -------------------------------------------------------------------------- --
-- Utiliza foldExp para hacer recursion sobre la expresion de forma tal que   --
-- frente a constructores de Diamond y Box sume una unidad a la visibilidad,  --
-- Var no aporte, en Not no se modifique, y que Or/And tome el maximo entre   --
-- las dos expresiones de ambos operadores involucrados.                      --
-- Ejemplo1: visibilidad (parse "p") = 0                                      --
-- Ejemplo2: visibilidad (parse "<>!<>p") = 2                                 --
-- Ejemplo3: visibilidad (parse "[](<>p && <>[]q)") = 3                       --
-- -------------------------------------------------------------------------- --
visibilidad :: Exp -> Integer
visibilidad = foldExp (const 0) id max max (+1) (+1)

-- Ejercicio 12
-- -------------------------------------------------------------------------- --
-- Utiliza foldExp para hacer recursion sobre la expresion de forma tal que   --
-- por cada constructor Var genere una lista con esa Prop y este es el unico  --
-- caso en que se agregan variables. Los constructores Not, Diamond y Box no  --
-- cambian las variables por lo cual se opera con la identidad. En el caso de --
-- Or/And podrian agregar repeticiones por lo cual se unen las variables      --
-- usando unionSR que utiliza listas por compresion para evitar esto.         -- 
-- Ejercicio1: extraer (parse "[](<>p && <>[]q)") = ["p","q"]                 --
-- Ejercicio1: extraer (parse "[](<>p && <>[]p)") = ["p"]                     --
-- -------------------------------------------------------------------------- --
extraer :: Exp -> [Prop]
extraer = foldExp (:[]) id unionSR unionSR id id
  where unionSR xs ys = xs ++ [y | y <- ys, not (y `elem` xs)]

-- Ejercicio 13
-- -------------------------------------------------------------------------- --
-- Explicacion bla bla bla porque eval' bla bla bla                           --
--                                                                            --
-- Ejemplos: Sea "ejKripke" un modelo que representa la figura 1 del TP       -- 
-- Ejemplo1: eval ejKripke 1 (parse "p && []q") = True                        --
-- Ejemplo2: eval ejKripke 1 (parse "p && <>r") = True                        --
-- Ejemplo3: eval ejKripke 1 (parse "[]r") = False                            --
-- -------------------------------------------------------------------------- --
eval :: Modelo -> Mundo -> Exp -> Bool
eval modelo mundo e = eval' modelo e mundo

eval' :: Modelo -> Exp -> Mundo -> Bool
eval' (K g fv) = foldExp fVarP fNot fOr fAnd fD fB
  where fVarP = (\p -> \mundo -> mundo `elem` (fv p))
        fNot = (\rec -> \mundo -> not (rec mundo))
        fOr = (\rec1 rec2 -> \mundo -> (rec1 mundo) || (rec2 mundo))
        fAnd = (\rec1 rec2 -> \mundo -> (rec1 mundo) && (rec2 mundo))
        fD = (\rec -> \mundo -> or [rec m | m <- (vecinos g mundo)])
        fB = (\rec -> \mundo -> and [rec m | m <- (vecinos g mundo)])
        
-- Ejercicio 14
-- -------------------------------------------------------------------------- --
-- Se utiliza eval para determinar durante la recursion de foldr si vale la   --
-- expresion en cada mundo del modelo y de ser asi lo incluye en la lista.    --
-- Ejemplos: Sea "ejKripke" un modelo que representa la figura 1 del TP       --
-- Ejemplo1: valeEn (parse "p && []q") ejKripke = [1]                         --
-- Ejemplo2: valeEn (parse "[]r") ejKripke = [2,3]                            --
-- -------------------------------------------------------------------------- --
valeEn :: Exp -> Modelo -> [Mundo]
valeEn e model@(K g r) = foldr (\mundo rec -> if (eval model mundo e) then (mundo:rec) else rec) [] (nodos g)

-- Ejercicio 15  
-- -------------------------------------------------------------------------- --
-- Se utiliza valeEn para calcular la lista de mundos en que no vale la       --
-- expresion, se eliminan estos mundos del grafo del modelo usando foldr con  --
-- sacarNodo y finalmente se devuelve un modelo con este nuevo grafo y la     --
-- misma relacion.                                                            --
-- De esta manera no modificamos el grafo antes de determinar efectivamente   --
-- cuales mundos no validan la expresion.                                     --
-- Ejemplos: Sea "ejKripke" un modelo que representa la figura 1 del TP y     --
--           "showGrafo" un proyector del grafo de un modelo                  --
-- Ejemplo1: showGrafo (quitar (parse "p && []q") ejKripke) =                 --
--[                                                                           --
-- 1 -> []                                                                    --
--]                                                                           --
-- -------------------------------------------------------------------------- --
quitar :: Exp -> Modelo -> Modelo
quitar e model@(K g r) = (K (foldr (\nodo rec -> sacarNodo nodo rec ) g (valeEn (Not e) model)) r)

-- Ejercicio 16
-- -------------------------------------------------------------------------- --
-- Se utiliza foldr y eval para evaluar la expresion en cada mundo del modelo --
-- y aplicando And a esta cadena se verifica si la expresion es valida en     --
-- todos ellos.                                                               --  
-- Ejemplos: Sea "ejKripke" un modelo que representa la figura 1 del TP       --
-- Ejemplo1:                                                                  --
--   cierto ejKripke (parse "p && []q") = False                               --
-- Ejemplo2:                                                                  --
--   cierto (quitar (parse "p && []q") ejKripke) (parse "p && []q") = True    --
-- -------------------------------------------------------------------------- --
cierto :: Modelo -> Exp -> Bool
cierto model@(K g r) e = foldr (\mundo rec -> (eval model mundo e) && rec) True (nodos g)
