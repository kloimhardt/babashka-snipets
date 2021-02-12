(ns scmtest.core
  (:require [sicmutils.value :as vl]
            [sicmutils.numsymb :as ny]
            [sicmutils.generic :as gn]
            [sicmutils.ratio :as rt]
            [sicmutils.structure :as st]
            [sicmutils.simplify :as sp] ;;necessary to load simplify multifunction
            [sicmutils.function :as fu] ;;necessary for ((gn/* 5 (fn[x] x)) 4)
            [sicmutils.expression :as ex]
            [sicmutils.differential :as dr]
            [sicmutils.numerical.minimize :as mn]
            [sicmutils.mechanics.lagrange :as lg]
            [sicmutils.expression.render :as render]
            [sicmutils.abstract.function :as af :include-macros true]))

(defn test-path
  "See p. 20"
  [t]
  (st/up (+ (* 4 t) 7)
      (+ (* 3 t) 5)
      (+ (* 2 t) 1)))

(defn la []
  (lg/Lagrangian-action (lg/L-free-particle 3)
                                  test-path
                                  0
                                  10))

(println "Lagrangian action")
(println (la))
(println "End")
