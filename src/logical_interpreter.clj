(ns logical-interpreter
	(:require [clojure.string :as str]
		  	  [clojure.java.io :as io])
)

(defn tieneParentesis1
	"Explicar"
	[x]
	(if (boolean (re-find (re-pattern "\\)") x))  true false)
)

(defn tieneParentesis2
	"Explicar"
	[x]
	(if (boolean (re-find (re-pattern "\\(") x))  true false)
)

(defn tienePunto
	"Explicar"
	[x]
	(if (boolean (re-find (re-pattern "\\.") x))  true false)
)

(defn mapeoSintaxis
	"Explicar"
	[x]
	(if (and (tieneParentesis1 x) (and (tieneParentesis2 x) (tienePunto x))) "bien" "mal")
)

(defn bddCorrecta?
	"Verifica que la sintaxis de la BDD sea correcta"
	[listaBdd]
	(if (= (some #{"mal"} (map mapeoSintaxis listaBdd)) "mal") false true)
)

(defn queryCorrecta?
	"Verifica que la sintaxis de la consulta sea correcta"
	[consulta]
	(if (and (tieneParentesis1 consulta) (tieneParentesis2 consulta)) true false)
)

(defn procesarBDD
	"Convierte la base de datos en una lista cuyos elementos son cada una de las lineas dadas"
	[bdd]
	(remove empty? (str/split-lines (str/replace  bdd #"[\t.]" "")))
)

(defn procesarBDD2
	"Convierte la base de datos en una lista cuyos elementos son cada una de las lineas dadas"
	[bdd]
	(remove empty? (str/split-lines bdd))
)

(defn mapeoTipo
	"Convierte la lista recibida de la bdd en una del tipo (Nombre Fact) o (Nombre Regla) según corresponda"
	[x]
	(if (= (re-find #":-" x) ":-") (str "Regla " (re-find #"^[^(]+"  x)) (str "Fact " (re-find #"^[^(]+"  x)))
)

(defn procesarTipos
	"Genera una lista para identificar si la query es un fact o una regla"
	[listaBdd]
	(remove empty? (distinct (map mapeoTipo listaBdd)))
)

(defn strip [coll chars]
  (apply str (remove #((set chars) %) coll)))

(defn mapeoRegla
	"Convierte la lista recibida de la bdd en una del tipo (Regla args facts)"
	[x]
	;(if (= (re-find #":-" x) ":-") (str/replace  x #"[(]" " (") "")
	(if (= (re-find #":-" x) ":-") x "")
)

(defn procesarReglas
	"Genera una lista para identificar los argumentos y facts de cada regla"
	[listaBdd]
	(remove empty? (distinct (map mapeoRegla listaBdd)))
)

(defn evaluarFact
	"Evalua si la definicion se encuentra en la base de datos"
	[consulta listaBdd]
	;(println "Es simple")
	(if (= (some #{consulta} listaBdd) consulta) true false)
)

(defn mapeoVariables
	"Genera el orden de las variables para acomodar los parametros"
	[x]
	(str ":" x)
)

(defn replace-map
  "given an input string and a hash-map, returns a new string with all
  keys in map found in input replaced with the value of the key"
  [s m]
  (clojure.string/replace s
              (re-pattern (apply str (interpose "|" (map #(java.util.regex.Pattern/quote %) (keys m)))))
          m))

(defn sustituirVariables 
	"Intercambia los X, Y, Z por los valores ingresados por el usuario"
	[factRegla variablesConsulta mapaVariables]
	(let [primerArg (get mapaVariables :X)
		  segundoArg (get mapaVariables :Y)
		  tercerArg (get mapaVariables :Z)]
	;(println factRegla)
	;(println variablesConsulta)
	;(println primerArg)
	;(println segundoArg)
	;(println tercerArg)
	(replace-map factRegla {"X" primerArg "Y" segundoArg "Z" tercerArg})
	)
)

(defn crearFact
	"Elabora el fact a cumplir a partir de la regla dada"
	[variablesConsulta factRegla ordenVariables]
	(let [nombreFact (subs factRegla 0 (str/index-of factRegla " "))
		  ordenFinal (map mapeoVariables (str/split ordenVariables #" "))
		  ;ordenFinal (vector (replace-map ordenVariables {"X" ":X" "Y" ":Y" "Z" ":Z"}))
		  ;ordenFinal2 [:Y :Z :X]
		  mapaVariables (zipmap [:X :Y :Z] (str/split variablesConsulta #" "))]
		;(println ordenVariables)
		;(println ordenFinal)
		(sustituirVariables factRegla variablesConsulta mapaVariables)
	)
)

(defn buscarRegla
	"Busca la difinicion de la regla ingresada como consulta para realizar la ejecucion de la misma"
	[consulta listaReglas]
	(let [nombreConsulta (subs consulta 0 (str/index-of consulta " "))
		  variablesConsulta (subs consulta (+(str/index-of consulta " ")1))
		  listadoFactsRegla (subs (first listaReglas) (+ (str/index-of (first listaReglas) ":") 3))
		  ordenVariables (strip (subs listadoFactsRegla (str/index-of listadoFactsRegla "(")) ".,()")
		  fact (crearFact variablesConsulta listadoFactsRegla ordenVariables)
		  ]
	;(println nombreConsulta)
	;(println variablesConsulta)
	;(println listadoFactsRegla)
	;(println ordenVariables)
	;(println fact)
	(crearFact variablesConsulta listadoFactsRegla ordenVariables)
	)
)

(defn esCompuesto?
	"Indica si la regla esta compuesta por un solo fact o mas"
	[facts]
	(boolean (str/index-of facts "),"))
)

(defn evaluarVariosFacts
	"Evalua todos los facts que compone la regla para verificar si se cumple o no"
	[facts listaBdd]
	(let [primerFact (subs facts 0 (+(str/index-of facts "),")1))
		  segundoFact (subs facts (+(str/index-of facts "),")3))]
	;(println "Es compuesta")
	;(println primerFact)
	;(println segundoFact)
	(and (evaluarFact primerFact listaBdd) (evaluarFact segundoFact listaBdd))
	)
)

(defn evaluarRegla
	"Evalua si la regla se encuentra en la base de datos"
	[consulta listaBdd listaReglas]
	(let [facts (buscarRegla (strip (str/replace  consulta #"[(]" " (") ".,()") listaReglas)]
	;(evaluarFact (buscarRegla (strip (str/replace  consulta #"[(]" " (") ".,()") listaReglas) listaBdd)
	;(println "Es regla")
	;(println facts)
	(if (esCompuesto? facts) (evaluarVariosFacts facts listaBdd) (evaluarFact facts listaBdd))
	)
)

(defn evaluarConsulta
	"Determina si la consulta del usuario debe devolver true o false"
	[consulta listaTipo listaBdd listaReglas]
	(if (boolean (some #{(str "Regla " (re-find #"^[^(]+"  consulta))} listaTipo)) (evaluarRegla consulta listaBdd listaReglas) (evaluarFact consulta listaBdd))
)

(defn evaluate-query
	"Evalua una consulta dada retornando true en caso de que se cumpla y false de lo contrario"
	[bdd consulta]
	(let [listaBdd (procesarBDD bdd)
		  listaTipo (procesarTipos listaBdd)
		  listaReglas (procesarReglas listaBdd)
		  listaVerificacion (procesarBDD2 bdd)]
		;(println (first listaBdd))
		;(println listaTipo)
		;(println listaReglas)
		(if (queryCorrecta? consulta)
			(if (bddCorrecta? listaVerificacion) (evaluarConsulta consulta listaTipo listaBdd listaReglas) nil)
			nil )
	)
)






















