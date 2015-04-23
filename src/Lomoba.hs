module Lomoba where
import Grafo
import Tipos


-- ---------------------------------Sección 6--------- Lomoba ---------------------------

-- Ejercicio 10
--foldExp toma una funcion por constructor del tipo Exp y recursiona
--sobre una instancia de Exp aplicando la funcion correspondiente
--sobre cada capa del mismo.
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
--visibilidad toma una instancia de tipo Exp y determina que tan 
--lejos deberiamos movernos en un grafo de Mundos para determinar la 
--validez de esa expresion. Utiliza foldExp para recursionar sobre 
--el parametro, de manera tal que frente a constructores de Diamond 
--y Box sume una unidad a la visibilidad, en Not no se modifique, 
--Var sea el caso base (const 0) y para Or/And tome el maximo de 
--visibilidad entre las dos expresiones involucradas.
visibilidad :: Exp -> Integer
visibilidad = foldExp (const 0) id max max (+1) (+1)

-- Ejercicio 12
--extraer toma una instancia de tipo Exp y devuelve una lista de todas las Props involucradas.
--Utiliza un foldExp para recursionar sobre el parametro, de manera tal que por cada capa Var genere una lista con esa Prop y luego vaya concatenando las listas resultantes en la recursion sacando repetidos mediante unionS(in)R(epetidos)
extraer :: Exp -> [Prop]
extraer = foldExp (:[]) id unionSR unionSR id id
  where unionSR xs ys = xs ++ [y | y <- ys, not (y `elem` xs)]

-- Ejercicio 13
--eval determina si la Expresion e es válida en el mundo de acuerdo al modelo pasado por parametro.
--Utiliza una función auxiliar eval' que hace uso de foldExp para recursionar sobre la expresión. 
--Lo complicado en esta función es determinar la evaluación cuando las expresiones tienen los constructores
--Box y Diamond porque es necesario verificar si las expresiones son válidas en alguno de los mundos vecinos.
--En una primera aproximación hicimos que la expresión recursiva (acumulador) de foldExp fuera un booleano.
--Esto nos trajo problemas al momento de enfrentarnos a los constructores antes mencionados ya que nos forzaba a
--hacer uso de recursión explicita para evaluar la expresión en los mundos vecinos.
--Para solucionar este problema hicimos que el acumulador de foldExp fuera una función que dado un mundo devolviera
--si la expresión era válida en dicho mundo. De este modo, fD y fB solo debían aplicar la función acumulada a los
--mundos vecinos y luego hacer un 'and' o un 'or' dependiendo el caso.

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
--valeEn retorna todos los mundos del grafo con el cual esta organizado el modelo en los cuales vale la expresion parametro.
--utiliza eval para determinar durante la recursion de foldr si vale la expresion en cada mundo y de ser asi lo incluye en la lista.
valeEn :: Exp -> Modelo -> [Mundo]
valeEn e model@(K g r) = foldr (\mundo rec -> if (eval model mundo e) then (mundo:rec) else rec) [] (nodos g)

-- Ejercicio 15
--quitar elimina todos los mundos del modelo en los cuales no vale la expresion parametro, devolviendo un nuevo Modelo donde su grafo no incluye dichos mundos.
--Recursiona sobre la lista 'precalculada' de mundos en los que no vale la expresion y los elimina del grafo utilizando sacarNodo. De esta manera no modificamos el grafo antes de determinar efectivamente cuales mundos no validan la expresion.
quitar :: Exp -> Modelo -> Modelo
quitar e model@(K g r) = (K (foldr (\nodo rec -> sacarNodo nodo rec ) g (valeEn (Not e) model)) r)

--quitar2 e model@(K g r) = (K (foldr (\nodo rec -> sacarNodo nodo rec ) g (valeEn (Not e) model)) (\x -> if ))

-- Ejercicio 16
--cierto determina si la expresion e es valida en el modelo, es decir, si vale para todos los mundos del mismo.
--evalua la expresion en cada mundo formando una cadena de ANDs de forma tal que si en algun mundo la expresion no es valida, el resultado sea false.
cierto :: Modelo -> Exp -> Bool
cierto model@(K g r) e = foldr (\mundo rec -> (eval model mundo e) && rec) True (nodos g)
