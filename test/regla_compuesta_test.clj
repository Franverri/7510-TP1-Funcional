(ns regla-compuesta-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))

(def list-bdd (vector "varon(juan)" "varon(pepe)" "mujer(maria)" "padre(juan, pepe)" "padre(juan, pepa"))

(deftest identificar-regla-compuesta-test
  (testing "Las regla hijo(X,Y) :- varon(X), hijo(Y, X) deben devoler true
  			(Se utiliza la parte de los facts de la relga)"
    (is (= (esCompuesto? "varon(X), hijo(Y, X)") true)
           true))
  (testing "Las regla prueba(X,Y,Z) :- uno(Z, Y, X) deben devoler false
  			(Se utiliza la parte de los facts de la relga)"
    (is (= (esCompuesto? "uno(Z, Y, X)") false)
           true))
)

(deftest evaluar-regla-compuesta-test
  (testing "La consulta hijo(pepe, juan) debe devoler true
  			Se utiliza la regla: hijo(X, Y) :- varon(X), padre(Y, X)"
    (is (= (evaluarVariosFacts "varon(juan), padre(juan, pepe)" list-bdd) true)
           true))
  (testing "La consulta hija(maria, juan) debe devoler false
  			Se utiliza la regla: hijo(X, Y) :- mujer(X), padre(Y, X)"
    (is (= (evaluarVariosFacts "mujer(maria), padre(juan, maria)" list-bdd) false)
           true))
)