(ns sicm-ch1
  (:refer-clojure :exclude [+ - * / = abs compare zero? ref partial
                            numerator denominator infinite?])
  (:require [emmy.env :as e :refer :all :exclude [F->C]]))

(defn define [h b]
  (list 'defn (ffirst h)  [(second (first h))]
        (list 'let-scheme (rest b))))

(define '((L-free-particle mass) local)
  '(let ((v (velocity local)))
    (* 1/2 mass (dot-product v v))))

(defn let-scheme [b e] (into [] (apply concat b)))

(let-scheme '((v (velocity local)))
  '(* 1/2 mass (dot-product v v)))

(defn f [x]
  (let [y 1]
    (defn g [z] (+ y z))
    (g x)))

(f 7)
(g 4)
