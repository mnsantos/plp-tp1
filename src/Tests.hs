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
