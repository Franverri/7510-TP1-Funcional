(ns sintaxis-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))

(deftest sintaxis-linea-test
  (testing "varon(juan). debe dar como resultado 'bien'"
    (is (= (mapeoSintaxis "varon(juan).") "bien")
           true))
  (testing "varon(juan) debe dar como resultado 'mal'"
    (is (= (mapeoSintaxis "varon(juan)") "mal")
           true))
  (testing "varon juan). debe dar como resultado 'mal'"
    (is (= (mapeoSintaxis "varon juan).") "mal")
           true))
  (testing "varon (juan . debe dar como resultado 'mal'"
    (is (= (mapeoSintaxis "varon(juan .") "mal")
           true))
)

(deftest sintaxis-consulta-test
  (testing "varon(juan) debe dar como resultado true"
    (is (= (queryCorrecta? "varon(juan)") true)
           true))
  (testing "varon juan debe dar como resultado false"
    (is (= (queryCorrecta? "varon juan") false)
           true))
)

(def list-bdd-correcta (vector "varon(juan)." "varon(pepe)." "mujer(maria)."))
(def list-bdd-incorrecta-1 (vector "varon(juan)" "varon(pepe)." "mujer(maria)."))
(def list-bdd-incorrecta-2 (vector "varon(juan)" "varon(pepe)." "mujer maria."))

(deftest sintaxis-listaBDD-test
  (testing "[varon(juan). varon(pepe). mujer(maria).] debe dar como resultado true"
    (is (= (bddCorrecta? list-bdd-correcta) true)
           true))
  (testing "[varon(juan) varon(pepe). mujer(maria).] debe dar como resultado false"
    (is (= (bddCorrecta? list-bdd-incorrecta-1) false)
           true))
  (testing "[varon(juan) varon(pepe). mujer maria.] debe dar como resultado false"
    (is (= (bddCorrecta? list-bdd-incorrecta-2) false)
           true))
)