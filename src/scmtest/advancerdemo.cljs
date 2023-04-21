(ns scmtest.advancerdemo
  (:require
    ;; sicmutils.env has aliases for common functions. If you are using a REPL, run
    ;; `sicmutils.env/bootstrap-repl!` to pull in all the aliases into the current namespace.
   [sicmutils.env :as s :include-macros true]
   [sicmutils.value :as vl]
   [sicmutils.generic :as gn]
   [sicmutils.structure :as st]
   [sicmutils.series :as sr]
   [sicmutils.operator :as op]
   [sicmutils.matrix :as mx]
   [sicmutils.mechanics.lagrange :as lg]))

(defn output [expr]
  ;; `simplify` collapses expressions while `->infix` prints them using Unicode operators.
  (let [string (s/->infix (s/simplify expr))]
    ;; Depending on your build options, the default output might go to the browser console or the
    ;; terminal's console.
    (println string)
    ;; Also hackishly append to the <body> for browser builds.
    (if (resolve 'js/document) (.append js/document.body string "\n"))))

(output "Running demo.minimal...\n")

;; It's important to use SICM's math operators so that the result remains a ratio.
(output (s/- (s/* 7 (s// 1 2)) 2))    ; 3/2
;; The default math operators will turn this expression into a float.
(output (- (* 7 (/ 1 2)) 2))          ; 1.5

;; Define a basic expression.
(def f (s/* 3 (s/expt s/sin 2)))
(output (f 'x))                       ; 3 sin²(x)
;; And take the derivative.
(output ((s/D f) 'x))                 ; 6 sin(x) cos(x)

;; More examples are available at /demo.clj

(output "1...\n")

(defn L-harmonic [m k]
  (fn [local]
  (let [q (s/coordinate local)
        v (s/velocity local)]
    (- (* (/ 1 2) m (s/square v)) (* (/ 1 2) k (s/square q))))))

(defn harmonic-state-derivative [m k]
  (s/Lagrangian->state-derivative (L-harmonic m k)))

(defn test-sa1 []
  (s/state-advancer harmonic-state-derivative 2.0 1.0))

(defn test-sa2 []
  ((s/state-advancer harmonic-state-derivative 2.0 1.0)
   (s/up 1.0 (s/up 1.0 2.0) (s/up 3.0 4.0))
   10.0 1.0e-3))

(comment

  (sicmutils.env/bootstrap-repl!)

  (state-advancer scmtest.advancerdemo/harmonic-state-derivative 2.0 1.0)

  ((state-advancer scmtest.advancerdemo/harmonic-state-derivative 2.0 1.0) (up 1.0 (up 1.0 2.0) (up 3.0 4.0)) 10.0 1.0e-3)

  :end)

(output "start to type\n")
