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
	"execEntity" ~: testsExecEntity
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
