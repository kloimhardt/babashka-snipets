(ns scmtest.scitest
  (:require [sci.core :as sci]
            [sicmutils.env.sci :as es]))

(println "loading scitest")
(.log js/console "scm + sci in the browser")

(def code "
(define-coordinates t R1-rect)

(def gamma (literal-manifold-map 'q R1-rect R2-rect))

(def the-metric (literal-metric 'g R2-rect))

(def Cartan
  (Christoffel->Cartan
   (metric->Christoffel-2
    the-metric
    (coordinate-system->basis R2-rect))))

(def geodesic-equation-residuals
  (((((covariant-derivative Cartan gamma) d:dt)
     ((differential gamma) d:dt))
    (chart R2-rect))
   ((point R1-rect) 't)))

(def simple-expression (* 3 'x))

[\"from within sci\"
 [Cartan (->TeX simple-expression)]]

")

(def st (atom nil))

(def res
  (try
    (sci/eval-string code
                     {:env st
                      :namespaces es/namespaces
                      :bindings (es/namespaces 'sicmutils.env)})
    (catch js/Error e
      (let [err-msg (.-message e)]
        (println "error")
        (println err-msg)
        (.log js/console "Error")
        (.log js/console (str err-msg)))
      nil)))

(println "result")
(println res)
(.log js/console "Result")
(.log js/console (str res))
