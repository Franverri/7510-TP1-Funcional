(ns cambio-variables-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))

(def list-reglas (vector "hijo(X, Y) :- varon(X), padre(Y, X)." "prueba(H, L) :- uno(L), dos(H, L)"))


(deftest encontrar-variables-regla-test
  (testing "Las variables de entrada de la regla hijo(X,Y) :- varon(X), hijo(Y, X) deben ser X e Y
  			Obtenidos de una lista que contiene las reglas de la BDD"
    (is (= (buscarVariables "hijo" list-reglas) (vector "X, Y"))
           true))
)

(deftest sustituir-variables-test
  (testing "Los facts de la consulta hijo(juan, pepe) a partir de la regla 'hijo(X, Y) :- varon(X), padre(Y, X).' 
  	debe quedar como 'varon(juan), padre(pepe, juan).'"
    (is (= (sustituirVariables "varon(X), padre(Y, X)" (vector "juan" "pepe") (vector "X" "Y"))
    	   "varon(juan), padre(pepe, juan)")
           true))
  (testing "Sustituir variables agregando mas argumentos y mas facts
  			Regla: prueba(X, Y, Z, K) :- uno(X), dos(Y, X), tres(K, Z, X)
  			prueba(messi, dybala, icardi, fazio) -> uno(messi), dos(dybala, messi), tres(fazio, icardi, messi)"
    (is (= (sustituirVariables "uno(X), dos(Y, X), tres(K, Z, X)" (vector "messi" "dybala" "icardi" "fazio") (vector "X" "Y" "Z" "K"))
    	   "uno(messi), dos(dybala, messi), tres(fazio, icardi, messi)")
           true))
)