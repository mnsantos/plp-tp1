import Grafo
import Tipos
import Lomoba
import Parser
import Test.HUnit

-- evaluar t para correr todos los tests
t = runTestTT allTests

allTests = test [
	"parser" ~: testsParser,
	"grafo" ~: testsGrafo
	]

testsParser = test [
	(Var "p") 						~=? (parse "p"),
	(And (Var "p") (Var "q")) 		~=? (parse "p && q"),
	(Or (Var "p") (Var "q")) 		~=? (parse "p || q"),
	(Or (Not (Var "p")) (Var "q"))	~=? (parse "!p || q"),
	(And (D (Var "p")) (Var "q")) 	~=? (parse "<>p && q"),
	(And (B (Var "p")) (Var "q")) 	~=? (parse "[]p && q"),
	(D (And (Var "p") (Var "q"))) 	~=? (parse "<>(p && q)"),
	(B (And (Var "p") (Var "q"))) 	~=? (parse "[](p && q)")]

testsGrafo = test [
	[1] ~~? (nodos (agNodo 1 vacio)),
	[1,2] ~~? (nodos (agNodo 2 (agNodo 1 vacio)))
	]

---------------
--  helpers  --
---------------

-- idem ~=? pero sin importar el orden
(~~?) :: (Ord a, Eq a, Show a) => [a] -> [a] -> Test
expected ~~? actual = (sort expected) ~=? (sort actual)
	where
		sort = foldl (\r e -> push r e) []
		push r e = (filter (e<=) r) ++ [e] ++ (filter (e>) r)
		
--------------		
-- Ejemplos --
--------------

ejFv :: Prop -> [Mundo]
ejFv "p" = [1]
ejFv "q" = [2,3]
ejFv "r" = [3]

ejGrafo :: Num a => Grafo a
ejGrafo = (agEje (1,3) (agEje (1,2) (agNodo 3(agNodo 2 (agNodo 1 vacio)))))

ejKripke :: Modelo
ejKripke = K ejGrafo ejFv

showGrafo :: Modelo -> Grafo Mundo
showGrafo (K g f) = g

--eval ejKripke 1 (parse "p && []q") = True
--eval ejKripke 1 (parse "p && <>r") = True
--eval ejKripke 1 (parse "[]r") = False

--visibilidad (parse "[](<>p && <>[]q)") = 3
--visibilidad (parse "[](<>p && <>[]p)") = 3

--extraer (parse "[](<>p && <>[]q)") = ["p","q"]
--extraer (parse "[](<>p && <>[]p)") = ["p"]

--valeEn (parse "p && []q") ejKripke = [1]
--valeEn (parse "[]r") ejKripke = [2,3]

--eval ejKripke 1 (parse "p && []q") = True
--eval ejKripke 2 (parse "p && []q") = False
--eval ejKripke 3 (parse "p && []q") = False
--showGrafo (quitar (parse "p && []q") ejKripke) = 
--[
-- 1 -> []
--]

--quitar2 FALLA ! Seria bueno analizar el porque !!!
--showGrafo (quitar2 (parse "p && []q") ejKripke) =
--[
-- 1 -> [3]
-- 3 -> []
--]

--cierto ejKripke (parse "p && []q") = False
--cierto (quitar (parse "p && []q") ejKripke) (parse "p && []q") = True
