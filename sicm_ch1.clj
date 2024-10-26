(ns sicm-ch1
  (:refer-clojure :exclude [+ - * / = abs compare zero? ref partial
                            numerator denominator infinite?])
  (:require [emmy.env :as e :refer :all :exclude [F->C]]))

(defn f [x]
  (let [y 1]
    (defn g [z] (+ y z))
    (g x)))

(f 7)
(g 4)

(comment
  (defmacro hu [x] (str x))
  ;; does not work
  ;; (hu pi/2)
  :end)

(* 1/2 pi)

(defmacro define [h b]
  (list 'defn (ffirst h) (into [] (rest (first h)))
        (list 'let-scheme (second b) (nth b 2))))

(defmacro let-scheme [b e] (list 'let (into [] (apply concat b)) e) )

(macroexpand
  '(define ((L-free-particle mass) local)
     (let ((v (velocity local)))
       (* 1/2 mass (dot-product v v)))))

(macroexpand
  '(let-scheme ((v (velocity local)))
    (* 1/2 mass (dot-product v v))))

(define ((L-free-particle mass local) local)
  (let ((v (velocity local)))
    (* 1/2 mass (dot-product v v))))

(L-free-particle 'm ['t 'x 'v])

(defn hu [h b]
  (if (coll? (first h))
    (let [hh (hu (first h) b)]
      (list 'def (second hh) (list 'fn (second (nth hh 2)) (list 'fn (into [] (rest h)) b))))
    (list 'def (first h) (list 'fn (into [] (rest h)) b))))

(macroexpand '(hu (a b) (identity 1)))
;; => (def a (fn [b] (identity 1)))

(hu [1 2] 5)
(hu [[1 2] 3] 5)
(hu [[[1 2] 3] 4] 5)
