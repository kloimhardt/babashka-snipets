(ns sicm-ch1
  (:refer-clojure :exclude [+ - * / = abs compare zero? ref partial
                            numerator denominator infinite?])
  (:require [emmy.env :as e :refer :all
             :exclude [F->C Lagrangian-action find-path Lagrange-equations p->r r->p
                       Lagrangian->state-derivative Lagrange-equations-first-order]]
            [emmy.mechanics.lagrange :as eml]
            [clojure.walk :as w]))

;; ~/Photon/babashka-snipets$ clj -Sdeps '{:deps {org.mentat/emmy {:mvn/version "0.32.0"} cider/cider-nrepl {:mvn/version "0.50.2"} }}' -M -m nrepl.cmdline --middleware "[cider.nrepl/cider-middleware]"

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

(defmacro let-scheme [b e] (list 'let (into [] (apply concat b)) e))

(defmacro define [h b]
  (let [body (w/postwalk-replace {'let 'let-scheme} b)]
    (if (coll? h)
      (if (coll? (first h))
        (list 'defn (ffirst h) (into [] (rest (first h)))
              (list 'fn (into [] (rest h)) body))
        (list 'defn (first h) (into [] (rest h))
              body))
      (list 'def h body))))

(def show-expression simplify)
(def velocities velocity)
(def  coordinates coordinate)

;; 1 Lagrangian Mechanics
;; 1.4 Computing Actions

(define ((L-free-particle mass) local)
  (let ((v (velocity local)))
    (* 1/2 mass (dot-product v v))))

((L-free-particle 'm) ['t 'x 'v])

(define q
  (up (literal-function 'x)
      (literal-function 'y)
      (literal-function 'z)))

((Gamma q) 't)

((compose (L-free-particle 'm) (Gamma q)) 't)

(define (Lagrangian-action L q t1 t2)
  (definite-integral (compose L (Gamma q)) t1 t2))

(define (test-path t)
  (up (+ (* 4 t) 7)
      (+ (* 3 t) 5)
      (+ (* 2 t) 1)))

(Lagrangian-action (L-free-particle 3.0)
                   test-path 0.0 10.0)

(define ((make-eta nu t1 t2) t)
  (* (- t t1) (- t t2) (nu t)))


(define ((varied-free-particle-action mass q nu t1 t2) eps)
  (let ((eta (make-eta nu t1 t2)))
    (Lagrangian-action (L-free-particle mass)
                       (+ q (* eps eta))
                       t1
                       t2)))

((varied-free-particle-action 3.0 test-path
                              (up sin cos square)
                              0.0 10.0)
 0.001)

(minimize
  (varied-free-particle-action 3.0 test-path
                               (up sin cos square)
                               0.0 10.0)
  -2.0 1.0)

(define ((parametric-path-action Lagrangian t0 q0 t1 q1) qs)
  (let ((path (eml/make-path t0 q0 t1 q1 qs)))
    (Lagrangian-action Lagrangian path t0 t1)))


(define (find-path Lagrangian t0 q0 t1 q1 n)
  (let ((initial-qs (linear-interpolants q0 q1 n)))
    (let ((minimizing-qs
            (multidimensional-minimize
              (parametric-path-action Lagrangian t0 q0 t1 q1)
              initial-qs)))
      (eml/make-path t0 q0 t1 q1 minimizing-qs))))

(define ((L-harmonic m k) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v)) (* 1/2 k (square q)))))

(define q-harmonic
  (find-path (L-harmonic 1.0 1.0) 0.0 1.0 (* 1/2 pi) 0.0 3))

(- (cos 0.8) (q-harmonic 0.8))

;; 1.5 The Euler–Lagrange Equations
;; 1.5.2 Computing Lagrange's Equations

(define ((Lagrange-equations Lagrangian) q)
  (- (D (compose ((partial 2) Lagrangian) (Gamma q)))
     (compose ((partial 1) Lagrangian) (Gamma q))))

(define (general-test-path t)
  (up (+ (* 'a t) 'a0)
      (+ (* 'b t) 'b0)
      (+ (* 'c t) 'c0)))

(((Lagrange-equations (L-free-particle 'm))
  general-test-path)
 't)

(show-expression
  (((Lagrange-equations (L-free-particle 'm))
    (literal-function 'x))
   't))

(define (proposed-solution t)
  (* 'A (cos (+ (* 'omega t) 'phi))))

(show-expression
  (((Lagrange-equations (L-harmonic 'm 'k))
    proposed-solution)
   't))


;; Exercise 1.11: Kepler's third law

;; Show that a planet in circular orbit satisfies Kepler's third law n^2a^3=G(M_1 + m_2), where n is the angular frequency of the orbit and a is the distance between sun and planet. (Hint: use the reduced mass to construct the Lagrangian)

(define ((L-Kepler-central-polar m V) local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (let ((r (ref q 0))     (phi (ref q 1))
          (rdot (ref qdot 0)) (phidot (ref qdot 1)))
      (- (* 1/2 m
            (+ (square rdot) (square (* r phidot))) )
         (V r)))))

(define ((gravitational-energy G m1 m2) r)
  (- (/ (* G m1 m2) r)))

(define (circle t)
  (up 'a (* 'n t)))

(define lagrangian-reduced
  (L-Kepler-central-polar (/ (* 'M_1 'm_2) (+ 'M_1 'm_2))
                          (gravitational-energy 'G 'M_1 'm_2)))

(show-expression
  (((Lagrange-equations lagrangian-reduced) circle) 't))


;; 1.6 How to find Lagrangians

(define ((L-uniform-acceleration m g) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (let ((y (ref q 1)))
      (- (* 1/2 m (square v)) (* m g y)))))

(show-expression
  (((Lagrange-equations
      (L-uniform-acceleration 'm 'g))
    (up (literal-function 'x)
        (literal-function 'y)))
   't))


(define ((L-central-rectangular m U) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v))
       (U (sqrt (square q))))))

(show-expression
  (((Lagrange-equations
      (L-central-rectangular 'm (literal-function 'U)))
    (up (literal-function 'x)
        (literal-function 'y)))
   't))


(show-expression
  (((Lagrange-equations
      (L-Kepler-central-polar 'm (literal-function 'U)))
    (up (literal-function 'r)
        (literal-function 'phi)))
   't))

;; 1.6.1 Coordinate Transformations

(define ((F->C F) local)
  (up (time local)
      (F local)
      (+ (((partial 0) F) local)
         (* (((partial 1) F) local)
            (velocity local)))))

(define (p->r local)
  (let ((polar-tuple (coordinate local)))
    (let ((r (ref polar-tuple 0))
          (phi (ref polar-tuple 1)))
      (let ((x (* r (cos phi)))
            (y (* r (sin phi))))
        (up x y)))))

(show-expression
  (velocity
    ((F->C p->r)
     (up 't (up 'r 'phi) (up 'rdot 'phidot)))))


(define (L-central-polar m U)
  (compose (L-central-rectangular m U) (F->C p->r)))


(show-expression
  ((L-central-polar 'm (literal-function 'U))
   (up 't (up 'r 'phi) (up 'rdot 'phidot))))

;; Coriolis and centrifugal forces

(define ((L-free-rectangular m) local)
  (let ((vx (ref (velocities local) 0))
        (vy (ref (velocities local) 1)))
    (* 1/2 m (+ (square vx) (square vy)))))

(define (L-free-polar m)
  (compose (L-free-rectangular m) (F->C p->r)))

(define ((F Omega) local)
  (let ((t (time local))
        (r (ref (coordinates local) 0))
        (theta (ref (coordinates local) 1)))
    (up r (+ theta (* Omega t)))))

(define (L-rotating-polar m Omega)
  (compose (L-free-polar m) (F->C (F Omega))))

(define (r->p local)
  (let ((rect-tuple (coordinate local)))
    (let ((x (ref rect-tuple 0))
          (y (ref rect-tuple 1)))
      (let ((r (sqrt (square rect-tuple)))
            (phi (atan (/ y x))))
        (up r phi)))))

(define (L-rotating-rectangular m Omega)
  (compose (L-rotating-polar m Omega) (F->C r->p)))

;; some error here
#_(show-expression
  ((L-rotating-rectangular 'm 'Omega)
   (up 't (up 'x_r 'y_r) (up 'xdot_r 'ydot_r))))

(+ (* 1/2 (expt 'Omega 2) 'm (expt 'x_r 2))
   (* 1/2 (expt 'Omega 2) 'm (expt 'y_r 2))
   (* -1 'Omega 'm 'xdot_r 'y_r)
   (* 'Omega 'm 'ydot_r 'x_r)
   (* 1/2 'm (expt 'xdot_r 2))
   (* 1/2 'm (expt 'ydot_r 2)))

;; error here
#_(show-expression
  (((Lagrange-equations (L-rotating-rectangular 'm 'Omega))
    (up (literal-function 'x_r) (literal-function 'y_r)))
   't))

(define x_r (literal-function 'x_r))

(define y_r (literal-function 'y_r))

(down
  (+ (* -1 (expt 'Omega 2) 'm (x_r 't))
     (* -2 'Omega 'm ((D y_r) 't))
     (* 'm (((expt D 2) x_r) 't)))
  (+ (* -1 (expt 'Omega 2) 'm (y_r 't))
     (* 2 'Omega 'm ((D x_r) 't))
     (* 'm (((expt D 2) y_r) 't))))

;; 1.6.2 Systems with Rigid Constraints
;; A pendulum driven at the pivot

(define ((T-pend m l g ys) local)
  (let ((t (time local))
        (theta (coordinate local))
        (thetadot (velocity local)))
    (let ((vys (D ys)))
      (* 1/2 m
         (+ (square (* l thetadot))
            (square (vys t))
            (* 2 l (vys t) thetadot (sin theta)))))))


(define ((V-pend m l g ys) local)
  (let ((t (time local))
        (theta (coordinate local)))
    (* m g (- (ys t) (* l (cos theta))))))

(define L-pendulum (- T-pend V-pend))

;; some error
#_(show-expression
  (((Lagrange-equations
      (L-pendulum 'm 'l 'g (literal-function 'y_s)))
    (literal-function 'theta))
   't))

;; 1.6.3 Constraints as Coordinate Transformations

;; L-uniform-acceleration already defined above
#_(define ((L-uniform-acceleration m g) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (let ((y (ref q 1)))
      (- (* 1/2 m (square v)) (* m g y)))))

(define ((dp-coordinates l y_s) local)
  (let ((t (time local))
        (theta (coordinate local)))
    (let ((x (* l (sin theta)))
          (y (- (y_s t) (* l (cos theta)))))
      (up x y))))

(define (L-pend m l g y_s)
  (compose (L-uniform-acceleration m g)
           (F->C (dp-coordinates l y_s))))
;;some error
#_(show-expression
  ((L-pend 'm 'l 'g (literal-function 'y_s))
   (up 't 'theta 'thetadot)))

;; 1.7 Evolution of Dynamical State

(define (Lagrangian->acceleration L)
  (let ((P ((partial 2) L)) (F ((partial 1) L)))
    (solve-linear-left
      ((partial 2) P)
      (- F
         (+ ((partial 0) P)
            (* ((partial 1) P) velocity))))))

(define (Lagrangian->state-derivative L)
  (let ((acceleration (Lagrangian->acceleration L)))
    (lambda (state)
            (up 1
                (velocity state)
                (acceleration state)))))

(define (harmonic-state-derivative m k)
  (Lagrangian->state-derivative (L-harmonic m k)))

(show-expression
  ((harmonic-state-derivative 'm 'k)
   (up 't (up 'x 'y) (up 'v_x 'v_y))))

(up 1 (up 'v_x 'v_y) (up (/ (* -1 'k 'x) 'm) (/ (* -1 'k 'y) 'm)))

(define ((Lagrange-equations-first-order L) q v)
  (let ((state-path (qv->state-path q v)))
    (- (D state-path)
       (compose (Lagrangian->state-derivative L)
                state-path))))

(define ((qv->state-path q v) t)
  (up t (q t) (v t)))

(show-expression
 (((Lagrange-equations-first-order (L-harmonic 'm 'k))
   (up (literal-function 'x)
       (literal-function 'y))
   (up (literal-function 'v_x)
       (literal-function 'v_y)))
  't))

;; Numerical integration
