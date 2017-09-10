(ns mapeo-tipo-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))

(def list-bdd (vector "varon(juan)." "mujer(maria)." "hijo(X, Y) :- varon(X), padre(Y, X)."))
(def list-bdd-repetidos (vector "varon(juan)." "varon(pepe)." "mujer(maria)." "mujer(agustina)." "hijo(X, Y) :- varon(X), padre(Y, X)."))
(def list-tipo-bdd (vector "Fact varon" "Fact mujer" "Regla hijo"))

(deftest tipo-linea-test
  (testing "list-bdd debe dar como resultado ['Fact varon' 'Fact mujer' 'Regla hijo']"
    (is (= (procesarTipos list-bdd) list-tipo-bdd)
           true))
  (testing "list-bdd-repetidos debe dar como resultado ['Fact varon' 'Fact mujer' 'Regla hijo']"
    (is (= (procesarTipos list-bdd) list-tipo-bdd)
           true))
)

(deftest regla-mapeo-test
  (testing "list-bdd debe dar como resultado ['hijo(X, Y) :- varon(X), padre(Y, X).'] es decir, sólo mantiene las reglas"
    (is (= (procesarReglas list-bdd) (vector "hijo(X, Y) :- varon(X), padre(Y, X)."))
           true))
)