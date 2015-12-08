import Routes
import Test.HUnit
import Data.List (sort)
import Data.Maybe (fromJust, isNothing	)

rutasFacultad = many [
  route ""             "ver inicio",
  route "ayuda"        "ver ayuda",
  scope "materia/:nombre/alu/:lu" $ many [
    route "inscribir"   "inscribe alumno",
    route "aprobar"     "aprueba alumno"
  ],
  route "alu/:lu/aprobadas"  "ver materias aprobadas por alumno"
  ]

rutasStringOps = route "concat/:a/:b" (\ctx -> (get "a" ctx) ++ (get "b" ctx))

-- evaluar t para correr todos los tests
t = runTestTT allTests

allTests = test [
	"patterns" ~: testsPattern,
	"matches" ~: testsMatches,
	"paths" ~: testsPaths,
	"eval" ~: testsEval,
	"evalWrap" ~: testsEvalWrap,
	"evalCtxt"~: testsEvalCtxt,
	"execEntity" ~: testsExecEntity,
	-- tests agregados --
	"split" ~: testSplit,
	"testPattern2" ~: testPattern2,
	"testGet" ~: testGet,
	"testMatches2" ~: testMatches2,
	"testPaths2" ~: testPaths2,
	"testEval2" ~: testEval2,
	"testWrap2" ~: testWrap2
	]

splitSlash = split '/'

testsPattern = test [
  splitSlash "" ~=? [],
	splitSlash "/" ~=? ["",""],
	splitSlash "/foo" ~=? ["", "foo"],
	pattern "" ~=? [],
	pattern "/" ~=? [],
	pattern "lit1/:cap1/:cap2/lit2/:cap3" ~=? [Literal "lit1", Capture "cap1", Capture "cap2", Literal "lit2", Capture "cap3"]
	]

testsMatches = test [
	Just (["tpf"],[("nombreMateria","plp")]) ~=? matches (splitSlash "materias/plp/tpf") (pattern "materias/:nombreMateria")
	]

path0 = route "foo" 1
path1 = scope "foo" (route "bar" 2)
path2 = scope "foo" (route ":bar" 3)
path3 = scope "" $ scope "" $ many [ scope "" $ route "foo" 1]

testsEvalCtxt = test [
	Just (1, []) ~=? eval path0 "foo",
	Just (2, []) ~=? eval path1 "foo/bar",
	isNothing (eval path1 "foo/bla") ~? "",
	Just (3, [("bar", "bla")]) ~=? eval path2 "foo/bla",
	Just (1, []) ~=? eval path3 "foo"
	]

path4 = many [
  (route "" 1),
  (route "lo/rem" 2),
  (route "ipsum" 3),
  (scope "folder" (many [
    (route "lorem" 4),
    (route "ipsum" 5)
    ]))
  ]


testsEval = test [
		1 ~=? justEvalP4 "",
		4 ~=? justEvalP4 "folder/lorem"
	]
	where justEvalP4 s = fst (fromJust (eval path4 s))

path410 = wrap (+10) path4

testsEvalWrap = test [
		14 ~=? justEvalP410 "folder/lorem"
	]
	where justEvalP410 s = fst (fromJust (eval path410 s))


-- ejempo donde el valor de cada ruta es una función que toma context como entrada.
-- para estos se puede usar en lugar además de eval, la función exec para devolver
-- la applicación de la función con sobre el contexto determinado por la ruta
rest entity = many [
  (route entity (const (entity++"#index"))),
  (scope (entity++"/:id") (many [
    (route "" (const (entity++"#show"))),
    (route "create" (\ctx -> entity ++ "#create of " ++ (get "id" ctx))),
    (route "update" (\ctx -> entity ++ "#update of " ++ (get "id" ctx))),
    (route "delete" (\ctx -> entity ++ "#delete of " ++ (get "id" ctx)))
    ]))
  ]

path5 = many [
  (route "" (const "root_url")),
  (rest "post"),
  (rest "category")
  ]

testsPaths = test [
 	sort ["","post","post/:id","post/:id/create","post/:id/update","post/:id/delete","category","category/:id","category/:id/create","category/:id/update","category/:id/delete"] ~=?
	 	sort (paths path5)
	]


testsExecEntity = test [
	Just "root_url" ~=? exec path5 "",
	Just "post#index" ~=? exec path5 "post",
	Just "post#show" ~=? exec path5 "post/35",
	Just "category#create of 7" ~=? exec path5 "category/7/create"
	]

-- Nuestros tests --

testSplit = test [
	splitSlash "/foo/bar/" ~=? ["", "foo", "bar", ""],
	splitSlash "//" ~=? ["","",""]
	]

testPattern2 = test [
	pattern "/lit1/:cap1/:cap2/lit2/:cap3/" ~=? [Literal "lit1", Capture "cap1", Capture "cap2", Literal "lit2", Capture "cap3"],
	pattern "" ~=? []
	]

testGet = test [
	get "nombre" [("nombre","plp"), ("lu","007−1")] ~=? "plp",
	get "n" [("l","plp"), ("lu","007−1"), ("n", "plp")] ~=? "plp",
	get "n" [("l","plp"), ("lu","007−1"), ("n", "plp1"), ("n", "plp2")] ~=? "plp1"
	]


testMatches2 = test [
	Just (["alu","007−1"] ,[("nombre","plp")]) ~=? matches ["materia","plp","alu","007−1"] [ Literal "materia",Capture "nombre"],
	Nothing ~=? matches ["otra","ruta"] [ Literal "ayuda"],
	Just([],[]) ~=? matches ["otra","ruta"] [ Literal "otra", Literal "ruta" ],
	Just([],[("n","mas")]) ~=? matches ["otra","ruta","mas"] [ Literal "otra", Literal "ruta", Capture "n" ],
	Nothing ~=? matches [] [Literal "algo"]
	]

path6 = many [
  route ""             "ver inicio",
  route "a"            "ver ayuda",
  scope "materia/:nombre/alu/:lu" $ many [
    route "inscribir"   "inscribe alumno",
    scope "a" $ many [
    	route "b" "",
    	route ":c" "hola"
    ],
    route "aprobar"     "aprueba alumno"
  ],
  route "alu/:lu/aprobadas"  "ver materias aprobadas por alumno"
  ]

testPaths2 = test [
	paths rutasFacultad ~=? ["","ayuda","materia/:nombre/alu/:lu/inscribir", "materia/:nombre/alu/:lu/aprobar","alu/:lu/aprobadas"],
	paths path6 ~=? ["","a","materia/:nombre/alu/:lu/inscribir","materia/:nombre/alu/:lu/a/b","materia/:nombre/alu/:lu/a/:c","materia/:nombre/alu/:lu/aprobar","alu/:lu/aprobadas"]
	]

testEval2 = test [
	Just ("ver materias aprobadas por alumno" ,[("lu","007−01")]) ~=? eval rutasFacultad "alu/007−01/aprobadas",
	Just ("hola",[("nombre","juan"),("lu","413-11"),("c","blabla")]) ~=? eval path6 "materia/juan/alu/413-11/a/blabla"
	]

testWrap2 = test [
	Just (True,[("nombre","juan"),("lu","413-11")]) ~=? eval (wrap null path6) "materia/juan/alu/413-11/a/b"
	]




