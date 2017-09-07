(ns logical-interpreter
	(:require [clojure.string :as str]
		  	  [clojure.java.io :as io])
)

(defn tieneParentesisFinal
	"Verifica si la linea posee parentesis de cierre"
	[x]
	(if (boolean (re-find (re-pattern "\\)") x))  true false)
)

(defn tieneParentesisInicial
	"Verifica si la linea posee parentesis de inicio"
	[x]
	(if (boolean (re-find (re-pattern "\\(") x))  true false)
)

(defn tienePunto
	"Verifica si la linea posee el punto final"
	[x]
	(if (boolean (re-find (re-pattern "\\.") x))  true false)
)

(defn mapeoSintaxis
	"Mapea las lineas de entrada con valores de salida del tipo 'bien' si la sintaxis es correcta o 'mal de lo contrario"
	[x]
	(if (and (tieneParentesisFinal x) (and (tieneParentesisInicial x) (tienePunto x))) "bien" "mal")
)

(defn bddCorrecta?
	"Verifica que la sintaxis de la BDD sea correcta"
	[listaBdd]
	(if (= (some #{"mal"} (map mapeoSintaxis listaBdd)) "mal") false true)
)

(defn queryCorrecta?
	"Verifica que la sintaxis de la consulta sea correcta"
	[consulta]
	(if (and (tieneParentesisFinal consulta) (tieneParentesisInicial consulta)) true false)
)

(defn procesarBDD
	"Convierte la base de datos en una lista cuyos elementos son cada una de las lineas dadas (eliminando los puntos)"
	[bdd]
	(remove empty? (str/split-lines (str/replace  bdd #"[\t.]" "")))
)

(defn procesarBDDconPuntos
	"Convierte la base de datos en una lista cuyos elementos son cada una de las lineas dadas (conservando los puntos)"
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
	(if (= (some #{consulta} listaBdd) consulta) true false)
)

(defn replace-map
  "Reemplaza los valores del string por aquellos que se pasen dentro de un mapa (La clave es reemplazada por su valor)"
  [s m]
  (clojure.string/replace s
              (re-pattern (apply str (interpose "|" (map #(java.util.regex.Pattern/quote %) (keys m)))))
          m))

(defn sustituirVariables 
	"Intercambia las variables por los valores ingresados por el usuario en la consulta"
	[factRegla variablesConsulta variablesRegla]
	(if (> (count variablesRegla) 0) (do (sustituirVariables (str/replace factRegla (first variablesRegla) (first variablesConsulta)) (rest variablesConsulta) (rest variablesRegla))) factRegla)
)

(defn crearFact
	"Elabora el/los fact/s a cumplir a partir de la regla dada"
	[variablesConsulta factRegla variablesRegla]
	(sustituirVariables factRegla variablesConsulta variablesRegla)
)

(defn encontrarVars
	"Funcion para lograr el mapeo correcto y encontrar las variables de la regla dada. Devuelve los args si encuentra la funcion y sino nil"
	[reglaCompleta nombreRegla]
	(if (= nombreRegla (subs reglaCompleta 0 (str/index-of reglaCompleta "("))) (subs reglaCompleta (+(str/index-of reglaCompleta "(")1) (str/index-of reglaCompleta ")")) nil)
)

(defn buscarVariables
	"Retorna las variables correspondientes a una regla dada"
	[nombre listaReglas]
	(remove nil? (map #(encontrarVars % nombre) listaReglas))

)

(defn buscarRegla
	"Busca la difinicion de la regla ingresada como consulta para realizar la ejecucion de la misma"
	[consulta listaReglas]
	(let [nombreConsulta (subs consulta 0 (str/index-of consulta " "))
		  variablesConsulta (str/split (subs consulta (+(str/index-of consulta " ")1)) #" ")
		  listadoFactsRegla (subs (first listaReglas) (+ (str/index-of (first listaReglas) ":") 3))
		  variablesRegla (str/split (first (buscarVariables nombreConsulta listaReglas)) #", ")]
	(crearFact variablesConsulta listadoFactsRegla variablesRegla)
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
	(let [listaFacts (str/split (replace-map facts {"), " ");"}) #";")]
	(every? true? (map #(evaluarFact % listaBdd) listaFacts))
	)
)

(defn evaluarRegla
	"Evalua si la regla se encuentra en la base de datos"
	[consulta listaBdd listaReglas]
	(let [facts (buscarRegla (strip (str/replace  consulta #"[(]" " (") ".,()") listaReglas)]
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
		  listaVerificacion (procesarBDDconPuntos bdd)]
		(if (queryCorrecta? consulta)
			(if (bddCorrecta? listaVerificacion) (evaluarConsulta consulta listaTipo listaBdd listaReglas) nil)
			nil )
	)
)






















