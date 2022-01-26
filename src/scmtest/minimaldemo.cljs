(ns scmtest.minimaldemo
  (:require
    ;; sicmutils.env has aliases for common functions. If you are using a REPL, run
    ;; `sicmutils.env/bootstrap-repl!` to pull in all the aliases into the current namespace.
   [sicmutils.env :as s :include-macros true]
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
(defn test-path-1
  "See p. 20"
  [t]
  (s/up (s/+ (s/* 4 t) 7)
        (s/+ (s/* 3 t) 5)
        (s/+ (s/* 2 t) 1)))

(output "2...\n")
(defn test-path-2
  "See p. 20"
  [t]
  (s/up (s/+ (s/* 1 t) 7)
        (s/+ (s/* 3 t) 5)
        (s/+ (s/* 2 t) 1)))

(output "3...\n")
(defn la1 []
  (lg/Lagrangian-action (lg/L-free-particle 3)
                        test-path-1
                        0
                        10))

(output "4...\n")
(defn la2 []
  (lg/Lagrangian-action (lg/L-free-particle 3)
                        test-path-2
                        0
                        10))

(output "5...\n")
(la1)

(output "6...\n")
;; (la2)

(output "start to type\n")
