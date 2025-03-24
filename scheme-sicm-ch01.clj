(ns sicm-ch1
  (:refer-clojure :exclude [+ - * / = abs compare zero? ref partial
                            numerator denominator infinite? time])
  (:require [emmy.env :as e :refer :all
             :exclude [F->C Lagrangian-action find-path Lagrange-equations p->r r->p
                       Lagrangian->state-derivative Lagrange-equations-first-order
                       Lagrangian->energy s->r Gamma-bar Euler-Lagrange-operator]]
            [emmy.mechanics.lagrange :as eml]
            [clojure.walk :as w]))

;; command to run this file:
;;$ clj -Sdeps '{:deps {org.mentat/emmy {:mvn/version "0.32.0"} cider/cider-nrepl {:mvn/version "0.50.2"} }}' -M sicm_ch1.clj

;; command to run a cider nrepl for development
;;$ clj -Sdeps '{:deps {org.mentat/emmy {:mvn/version "0.32.0"} cider/cider-nrepl {:mvn/version "0.50.2"} }}' -M -m nrepl.cmdline --middleware "[cider.nrepl/cider-middleware]"

(defn walk [inner outer form]
  (cond
    (list? form) (outer (apply list (map inner form)))
    (seq? form)  (outer (doall (map inner form)))
    (coll? form) (outer (into (empty form) (map inner form)))
    :else        (outer form)))
(defn postwalk [f form]
  (walk (partial postwalk f) f form))
(defn postwalk-replace [smap form]
  (postwalk (fn [x] (if (contains? smap x) (smap x) x)) form))
(defmacro let-scheme [b & e]
  (concat (list 'let (into [] (apply concat b))) e))
(defmacro define-1 [h & b]
  (let [body (postwalk-replace {'let 'let-scheme} b)]
    (if (coll? h)
      (if (coll? (first h))
        (list 'defn (ffirst h) (into [] (rest (first h)))
              (concat (list 'fn (into [] (rest h))) body))
        (concat (list 'defn (first h) (into [] (rest h)))
                body))
      (concat (list 'def h) body))))
(defmacro define [h & b]
  (if (and (coll? h) (= (first h) 'tex-inspect))
    (list 'do
          (concat ['define-1 (second h)] b)
          h)
    (concat ['define-1 h] b)))
(defmacro lambda [h b]
  (list 'fn (into [] h) b))
(def show-expression simplify)
(def velocities velocity)
(def coordinates coordinate)
(def vector-length count)
(defn time [state] (first state))

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

(show-expression
  ((L-rotating-rectangular 'm 'Omega)
   (up 't (up 'x_r 'y_r) (up 'xdot_r 'ydot_r))))

(+ (* 1/2 (expt 'Omega 2) 'm (expt 'x_r 2))
   (* 1/2 (expt 'Omega 2) 'm (expt 'y_r 2))
   (* -1 'Omega 'm 'xdot_r 'y_r)
   (* 'Omega 'm 'ydot_r 'x_r)
   (* 1/2 'm (expt 'xdot_r 2))
   (* 1/2 'm (expt 'ydot_r 2)))

(show-expression
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

(show-expression
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

(show-expression
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

(define ((qv->state-path q v) t)
  (up t (q t) (v t)))

(define ((Lagrange-equations-first-order L) q v)
  (let ((state-path (qv->state-path q v)))
    (- (D state-path)
       (compose (Lagrangian->state-derivative L)
                state-path))))

(show-expression
 (((Lagrange-equations-first-order (L-harmonic 'm 'k))
   (up (literal-function 'x)
       (literal-function 'y))
   (up (literal-function 'v_x)
       (literal-function 'v_y)))
  't))

;; Numerical integration

((state-advancer harmonic-state-derivative 2.0 1.0)
 (up 1.0 (up 1.0 2.0) (up 3.0 4.0))
 10.0
 1.0e-12)


(up 11.0
    (up 3.7127916645844437 5.420620823651583)
    (up 1.6148030925459782 1.8189103724750855))


(define ((periodic-drive amplitude frequency phase) t)
  (* amplitude (cos (+ (* frequency t) phase))))


(define (L-periodically-driven-pendulum m l g A omega)
  (let ((ys (periodic-drive A omega 0)))
    (L-pend m l g ys)))

(show-expression
  (((Lagrange-equations
      (L-periodically-driven-pendulum 'm 'l 'g 'A 'omega))
    (literal-function 'theta))
   't))

(define (pend-state-derivative m l g A omega)
  (Lagrangian->state-derivative
    (L-periodically-driven-pendulum m l g A omega)))

(show-expression
  ((pend-state-derivative 'm 'l 'g 'A 'omega)
   (up 't 'theta 'thetadot)))

;; 1.8 Conserved Quantities
;; 1.8.2 Energy Conservation

(define (Lagrangian->energy L)
  (let ((P ((partial 2) L)))
    (- (* P velocity) L)))

;; 1.8.3 Central Forces in Three Dimensions

(define ((T3-spherical m) state)
  (let ((q (coordinate state))
        (qdot (velocity state)))
    (let ((r (ref q 0))
          (theta (ref q 1))
          (rdot (ref qdot 0))
          (thetadot (ref qdot 1))
          (phidot (ref qdot 2)))
      (* 1/2 m
         (+ (square rdot)
            (square (* r thetadot))
            (square (* r (sin theta) phidot)))))))

(define (L3-central m Vr)
  (define (Vs state)
    (let ((r (ref (coordinate state) 0))) (Vr r)))
  (- (T3-spherical m) Vs))

(show-expression
  (((partial 1) (L3-central 'm (literal-function 'V)))
   (up 't
       (up 'r 'theta 'phi)
       (up 'rdot 'thetadot 'phidot))))

(show-expression
  (((partial 2) (L3-central 'm (literal-function 'V)))
   (up 't
       (up 'r 'theta 'phi)
       (up 'rdot 'thetadot 'phidot))))

(define ((ang-mom-z m) rectangular-state)
  (let ((xyz (coordinate rectangular-state))
        (v (velocity rectangular-state)))
    (ref (cross-product xyz (* m v)) 2)))


(define (s->r spherical-state)
  (let ((q (coordinate spherical-state)))
    (let ((r (ref q 0))
          (theta (ref q 1))
          (phi (ref q 2)))
      (let ((x (* r (sin theta) (cos phi)))
            (y (* r (sin theta) (sin phi)))
            (z (* r (cos theta))))
        (up x y z)))))


(show-expression
  ((compose (ang-mom-z 'm) (F->C s->r))
   (up 't
       (up 'r 'theta 'phi)
       (up 'rdot 'thetadot 'phidot))))


(show-expression
  ((Lagrangian->energy (L3-central 'm (literal-function 'V)))
   (up 't
       (up 'r 'theta 'phi)
       (up 'rdot 'thetadot 'phidot))))

;; 1.8.4 The Restricted Three-Body Problem

(define ((L0 m V) local)
  (let ((t (time local))
        (q (coordinates local))
        (v (velocities local)))
    (- (* 1/2 m (square v)) (V t q))))

(define ((V a GM0 GM1 m) t xy)
  (let ((Omega (sqrt (/ (+ GM0 GM1) (expt a 3))))
        (a0 (* (/ GM1 (+ GM0 GM1)) a))
        (a1 (* (/ GM0 (+ GM0 GM1)) a)))
    (let ((x (ref xy 0)) (y (ref xy 1))
          (x0 (* -1 a0 (cos (* Omega t))))
          (y0 (* -1 a0 (sin (* Omega t))))
          (x1 (* +1 a1 (cos (* Omega t))))
          (y1 (* +1 a1 (sin (* Omega t)))))
      (let ((r0
              (sqrt (+ (square (- x x0)) (square (- y y0)))))
            (r1
              (sqrt (+ (square (- x x1)) (square (- y y1))))))
        (- (+ (/ (* GM0 m) r0) (/ (* GM1 m) r1)))))))

(define ((LR3B m a GM0 GM1) local)
  (let ((q (coordinates local))
        (qdot (velocities local))
        (Omega (sqrt (/ (+ GM0 GM1) (expt a 3))))
        (a0 (* (/ GM1 (+ GM0 GM1)) a))
        (a1 (* (/ GM0 (+ GM0 GM1)) a)))
    (let ((x (ref q 0))     (y (ref q 1))
          (xdot (ref qdot 0)) (ydot (ref qdot 1)))
      (let ((r0 (sqrt (+ (square (+ x a0)) (square y))))
            (r1 (sqrt (+ (square (- x a1)) (square y)))))
        (+ (* 1/2 m (square qdot))
           (* 1/2 m (square Omega) (square q))
           (* m Omega (- (* x ydot) (* xdot y)))
           (/ (* GM0 m) r0) (/ (* GM1 m) r1))))))

(define ((LR3B1 m a0 a1 Omega GM0 GM1) local)
  (let ((q (coordinates local))
        (qdot (velocities local)))
    (let ((x (ref q 0))     (y (ref q 1))
          (xdot (ref qdot 0)) (ydot (ref qdot 1)))
      (let ((r0 (sqrt (+ (square (+ x a0)) (square y))))
            (r1 (sqrt (+ (square (- x a1)) (square y)))))
        (+ (* 1/2 m (square qdot))
           (* 1/2 m (square Omega) (square q))
           (* m Omega (- (* x ydot) (* xdot y)))
           (/ (* GM0 m) r0) (/ (* GM1 m) r1))))))

((Lagrangian->energy (LR3B1 'm 'a_0 'a_1 'Omega 'GM_0 'GM_1))
 (up 't (up 'x_r 'y_r) (up 'vx_r 'vy_r)))

(+ (* 1/2 'm (expt 'vx_r 2))
   (* 1/2 'm (expt 'vy_r 2))
   (/ (* -1 'GM_0 'm)
      (sqrt (+ (expt (+ 'x_r 'a_0) 2) (expt 'y_r 2))))
   (/ (* -1 'GM_1 'm)
      (sqrt (+ (expt (- 'x_r 'a_1) 2) (expt 'y_r 2))))
   (* -1/2 'm (expt 'Omega 2) (expt 'x_r 2))
   (* -1/2 'm (expt 'Omega 2) (expt 'y_r 2)))

;; 1.8.5 Noether’s Theorem

(define (F-tilde angle-x angle-y angle-z)
  (compose (Rx angle-x) (Ry angle-y) (Rz angle-z) coordinate))

;; L-central-rectangular is already defined in chapter 1.6
#_(define ((L-central-rectangular m U) state)
  (let ((q (coordinate state))
        (v (velocity state)))
    (- (* 1/2 m (square v))
       (U (sqrt (square q))))))

(define the-Noether-integral
  (let ((L (L-central-rectangular
             'm (literal-function 'U))))
    (* ((partial 2) L) ((D F-tilde) 0 0 0))))

(show-expression
  (the-Noether-integral
    (up 't
        (up 'x 'y 'z)
        (up 'vx 'vy 'vz))))

(down (+ (* 'm 'vy 'z) (* -1 'm 'vz 'y))
      (+ (* 'm 'vz 'x) (* -1 'm 'vx 'z))
      (+ (* 'm 'vx 'y) (* -1 'm 'vy 'x)))

;; 1.9 Abstraction of Path Functions

(define ((Gamma-bar f-bar) local)
  ((f-bar (osculating-path local)) (time local)))

(define (F->C1 F)
  (define (C local)
    (let ((n (vector-length local)))
      (define (f-bar q-prime)
        (define q
          (compose F (Gamma q-prime)))
        (Gamma q n))
      ((Gamma-bar f-bar) local)))
  C)

(define (Euler-Lagrange-operator L)
  (- (Dt ((partial 2) L)) ((partial 1) L)))

((Euler-Lagrange-operator
   (L-harmonic 'm 'k))
 (up 't 'x 'v 'a))

(+ (* 'a 'm) (* 'k 'x))

((compose
   (Euler-Lagrange-operator (L-harmonic 'm 'k))
   (Gamma (literal-function 'x) 4))
 't)

(+ (* 'k ((literal-function 'x) 't))
   (* 'm (((expt D 2) (literal-function 'x)) 't)))
