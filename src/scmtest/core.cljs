(ns scmtest.core
  (:refer-clojure :exclude [+ * / -])
  (:require [sicmutils.value :as vl]
            [sicmutils.numsymb :as ny]
            [sicmutils.generic :as gn :refer [+ * / -]]
            [sicmutils.ratio :as rt]
            [sicmutils.structure :as st]
            [sicmutils.simplify :as sp]
            [sicmutils.function :as fu]
            [sicmutils.expression :as ex]
            [sicmutils.differential :as dr]
            [sicmutils.numerical.minimize :as mn]
            [sicmutils.mechanics.lagrange :as lg]
            [sicmutils.expression.render :as render]
            [sicmutils.abstract.function :as af :include-macros true]))

(println "loading core!")

(defn test-path-1
  "See p. 20"
  [t]
  (st/up (+ (* 4 t) 7)
      (+ (* 3 t) 5)
      (+ (* 2 t) 1)))

(defn test-path-2
  "See p. 20"
  [t]
  (st/up (+ (* 1 t) 7)
         (+ (* 3 t) 5)
         (+ (* 2 t) 1)))

(defn la1 []
  (lg/Lagrangian-action (lg/L-free-particle 3)
                                  test-path-1
                                  0
                                  10))

(defn la2 []
  (lg/Lagrangian-action (lg/L-free-particle 3)
                        test-path-2
                        0
                        10))

(println "start to type!")
