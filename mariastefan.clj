;; # Table of Contents
;; ### 1 Code Preamble
;; ### 2 Josef Stefan's T^4 law
;; ### 3 Small doodley pieces
;; ### 4 deBroglie Wavelength

;; # 1 Code Präambel (bitte runterscrollen zum Hauptteil)
(ns mariastefan
  (:refer-clojure :exclude [+ - * / = abs compare zero? ref partial
                            numerator denominator infinite?])
  (:require [emmy.env :as e :refer :all :exclude [F->C]]
            [scicloj.kindly.v4.api :as kindly]
            [scicloj.kindly.v4.kind :as kind]))

(def html kind/hiccup)

(defn is-within! [a [b db]]
  (when (< (- b db) (simplify a) (+ b db)) a))

(defn is-equal! [a b]
  (when (zero? (simplify (- a b))) a))

(defn solves! [x f]
  (when (zero? (simplify (f x))) x))

(defn gives-zero! [f x]
  (when (zero? (simplify (f x))) x))

(defn postfix? [ctx ex]
  ((-> ctx :Schlusselworte :postfix)
   (str (last ex))))

(defn infix? [ctx ex]
  ((-> ctx :Schlusselworte :infix)
   (str (second ex))))

(defn infix-function? [ctx ex]
  ((-> ctx :Schlusselworte :infix-function) (str (second ex))))

(defn ifx-fn-mp-decon? [ctx ex]
  (and (infix-function? ctx ex)
       ((-> ctx :Schlusselworte :infix-function-map-deconstrucion)
        (str (second (last ex))))))

(defn ifx-fn-reverse? [ctx ex]
  (and (infix-function? ctx ex)
       ((-> ctx :Schlusselworte :infix-function-reverse)
        (str (second (last ex))))))

(defn notext? [ctx smb]
  ((-> ctx :Schlusselworte :notext)
   (str smb)))

(defn npow [x n]
  (apply * (repeat n x)))

(defn mypow [x n]
  (if (integer? n)
    (npow x n)
    (exp (* (log x) n))))

(defn n-transpose [m]
  (apply mapv vector m))

(defn r-transpose [x]
  (reverse (n-transpose x)))

(defn mround [x] (floor (+ x 0.5)))

(defn to-double [x]
  (let [n (exp -30)]
    (- (+ x n) n)))

(defn round [x dec]
  (to-double (/ (mround (* x (mypow 10 dec))) (mypow 10 dec))))

(defn str-to-int [i]
  ;; in maria.cloud
  #_(int i)
  (Integer/parseInt i))

(def plus +)
(def minus -)
(def mal *)
(def von *)
(def dot *)
(def durch /)
(def hoch mypow)
(def null 0)
(def Ein 1)
(def Milliard (fn [x] (* x (mypow 10 9))))
(def stel (fn [x y] (/ x y)))
(defn Komma [& lst]
  (let [a (apply str (rest lst))]
    (to-double (+ (first lst) (/ (str-to-int a) (mypow 10 (count a)))))))

(defn sw [ctx ex]
  (cond
    (not (coll? ex)) ex
    (ifx-fn-mp-decon? ctx ex)
    (list (list 'fn [{:keys (first (last ex))}] (first ex))
          (last (last ex)))
    (ifx-fn-reverse? ctx ex)
    (list (list 'fn [(last (last ex))] (first ex))
          (first (last ex)))
    (infix-function? ctx ex)
    (list (list 'fn [(first (last ex))] (first ex))
          (last (last ex)))
    (postfix? ctx ex)
    (conj (butlast ex) (last ex))
    (infix? ctx ex)
    (conj (rest (rest ex)) (first ex) (second ex))
    :else            ex))

(defn bx [ctx ex]
  (if (not (coll? ex)) ex
      (str "\\fbox{"
           (apply str
                  (interpose " " (remove #(notext? ctx %) ex)))
           "}")))

(defn maxcount [xs mini]
  (apply max (conj (map #(-> (second %) :style :count) (filter coll? xs)) mini)))

(defn b [p] {:style {:border "1px solid gray" :padding (str p "px") :count p}})

(defn hx [ctx ex]
  (if (not (coll? ex))
    (str " " ex " ")
    (let [rex (remove #(notext? ctx %) ex)]
      (into [:span (b (+ 3 (maxcount rex 2)))] rex))))

(defn table-formula? [ctx ex]
  (and (coll? (last ex))
       ((-> ctx :Schlusselworte :infix-function) (str (first (last ex))))))

(defn threadzero [ctx ex]
  (if (table-formula? ctx ex)
    (if (> (count ex) 2)
      (cons (threadzero ctx (butlast ex)) (last ex))
      (cons (first ex) (last ex)))
    ex))

(def gctx {:Schlusselworte
           {:infix
            #{"plus" "minus" "mal" "von" "dot" "durch" "hoch" "Ein" "Milliard"
              "Komma"}
            :postfix
            #{"stel"}
            :infix-function
            #{"mit" "und"}
            :infix-function-map-deconstrucion
            #{"aus"}
            :infix-function-reverse
            #{"für"}
            :notext
            #{"dot"}}
           :Schulwissen
           {:e  euler
            :pi pi}})

(defn my-walk
  [inner outer form]
  (cond
    (list? form)      (outer (apply list (map inner form)))
    (map-entry? form) form
    (seq? form)       (outer (doall (map inner form)))
    (record? form)    (outer (reduce (fn [r x] (conj r (inner x))) form form))
    (coll? form)      (outer (into (empty form) (map inner form)))
    :else             (outer form)))

(defn postwalk
  [f form]
  (my-walk (partial postwalk f) f form))

(defmacro calcbox [ex & dbg]
  (let [swe (postwalk #(sw gctx %) (threadzero gctx ex))
        bxe (if (vector? ex)
              (->> (cons (first ex) (apply concat (rest ex)))
                   (map (fn [mex] (postwalk #(bx gctx %) mex)))
                   (interpose " \\\\ ")
                   (apply str))
              (postwalk #(bx gctx %) ex))
        hxe (if (vector? ex)
              (->> (cons (first ex) (apply concat (rest ex)))
                   (map (fn [mex] (postwalk #(hx gctx %) mex)))
                   (map #(vector :p [:div %]))
                   (into [:p]))
              [:p (postwalk #(hx gctx %) ex)])]
    {:code   `'~swe
     :calc   (if (seq dbg) (into [] dbg) swe)
     :tex    bxe
     :hiccup hxe}))

;; # 2 J. Stefan: Über die Beziehung zwischen der Wärmestrahlung und der Temperatur

;;Temperatur in Celsius
(def temper [80 100 120 140 160 180 200 220 240])

;; Dulong und Petit angegebene Messdaten (extrapoliert aus Messungen)
(def dp-meas [1.74 2.30 3.02 3.88 4.89 6.10 7.40 8.81 10.69])

;; von D&P angegeben als nach ihrer Heuristik gerechnet
(def dp-theor [1.72 2.33 3.05 3.89 4.87 6.03 7.34 8.89 10.68])

;; vom Stefan berechnete Werte
(def st-theor [1.66 2.30 3.05 3.92 4.93 6.09 7.42 8.92 10.62])

(defn dp-formel [Celsius]
  (calcbox [((((A hoch x) minus 1) mal M)
             mit
             [x in Celsius])
            [und [(1 Komma 0 0 77) für A]]
            [und [(2 Komma 0 2) für M]]]))
(html (:hiccup (dp-formel 0)))

(def tb2 (map (fn [[T m t]]
                [T m (round (- t m) 2) t (round (:calc (dp-formel T)) 2)])
              (n-transpose [temper dp-meas dp-theor])))

;; Temperatur Messwert Differenz RechenWert_D&P Meine-Rechnung

tb2

(defn stefan-formel [Celsius]
  (calcbox [((((T plus x) hoch 4 )
              minus
              (T hoch 4))
             mal
             B)
            [mit [x in Celsius]]
            [und [(Ein (6 Milliard) stel) für B]]
            [und [273 für T]]]))
(html (:hiccup (stefan-formel 0)))

(def pow_273_4 (calcbox [(((T mal T) mal T) mal T) [mit [T gleich 273]]]))
(html (:hiccup pow_273_4))

(:calc pow_273_4)

;; Unten passt die Differenz mit Paper überein, auch Meine-Rechnung lt. Paper gerechnet passt
(def tb7 (map (fn [[T m st-t]]
                [T m (round (- st-t m) 2) st-t
                 (round (:calc (stefan-formel T)) 2)])
              (n-transpose [temper dp-meas st-theor])))

;; Temperatur Messwert Differenz Rechenwert_St Meine-Rechnung
tb7

(defn minsec [sec]
  (mapv mround [(quot sec 60) (mod sec 60)]))

(defn speed->sec [sp]
  (* (/ 20.0 sp) 60))

(defn tb3 [tbl]
  (map (fn [tb]
         (map #(minsec (speed->sec %)) tb))
       tbl))

;; Umrechnung von Messwerten und Rechnungen nach Zeitdauer -- MinutenSekunden, Absteigende Temperatur

(tb3 (r-transpose [dp-meas dp-theor st-theor]))

(def Minuten 'Minuten)
(def Minute 'Minute)
(def Sekunden 'Sekunden)
(def Sekunde 'Sekunde)
(def Eine 'Eine)
(def und 'und)
(def ist 'ist)
(def Null 'Null)

(defn text-minute [m]
  (case m
    0 [Null Minuten]
    1 [Eine Minute]
    [m Minuten]))

(defn text-sekunde [m]
  (case m
    0 [Null Sekunden]
    1 [Eine Sekunde]
    [m Sekunden]))

(defn tb4 [tb]
  (mapv (fn [[m s]] [(text-minute m) und (text-sekunde s)])
        tb))

;; Zeitdauer der Messwerte in Worten
(tb4 (map first (tb3 (r-transpose [dp-meas]))))

(def minus- 'minus)
(def plus+ 'plus)

(defn tb6 [tb]
  (mapv (fn [[[am as] [bm bs]]]
          [[bs
            (if (neg? (- as bs)) minus- plus+)
            (abs (- as bs))]
           ist
           as])
        tb))
;; Differenz der Sekunden: D&P-Rechnung Differenz D&P-Messung
(tb6 (tb3 (r-transpose [dp-meas dp-theor])))

;; Differenz der Sekunden: St-Rechnung Differenz D&P-Messung
(tb6 (tb3 (r-transpose [dp-meas st-theor])))

;; # 3 Kleine Fingerübungen

(def bsp0 (calcbox (1 plus 3)))
(html (:hiccup bsp0))

(:calc bsp0)

;; praktisch zum debuggen: der generierte code
(:code bsp0)

(def bsp1 (calcbox (2 plus 1) "andere Rechnung" (+ 4 5)))
(html (:hiccup bsp1))

;; noch praktischer zum debuggen: calculation unterdrücken
(:calc bsp1)

(def bsp2 (calcbox ((X plus 1) mit [X gleich [(Y plus 2) mit [Y gle=ich 3]]])))
(html (:hiccup bsp2))

(:calc bsp2)

(def bsp3 (calcbox ( [(X plus Y) mit [X ist-gleich 3]] mit [Y ist 2]) ))
;; es ist wurst ob glei=ch, ist-gleich, -in-: nur ein Füllwort
(html (:hiccup bsp3))

(:calc bsp3)

(defn bsp4 [Sekunden Y]
  (calcbox ((X plus Y) mit [X -in- Sekunden])))

(html (:hiccup (bsp4 0 0)))

(:calc (bsp4 5 6))

(defn bsp5 [{:keys [Schulwissen]}]
  (calcbox ((e hoch pi) mit [[e pi] aus Schulwissen])))
(html (:hiccup (bsp5 gctx)))

(def bsp6 (calcbox [(X plus Y) [mit [X gleich (Y plus 7)]] [mit [Y gle=ich 3]]]))
(html (:hiccup bsp6))

;; # 4 deBroglie Wavelength

;; ## Internal Vibarations
;; as always with Einstein, we start with E = mc^2
(defn E0 [m c] (* m c c))

;; deBroglies first hypothesis was to assume that every particle has a hypothetical
;; internal vibration at frequency nu0 which relates to the rest energy
;; in rest frame of particle (only there this energy-frequency relation holds)
(defn nu_naught [E0] (/ E0 'h))

;; particle travels at velocity v
(defn v [beta c] (* beta c))
(defn gamma [beta] (/ 1 (sqrt (- 1 (* beta beta)))))

;; time dilation: internal vibration is slower for observer
(defn nu_one [nu_naught gamma] (/ nu_naught gamma))

;; sine formula for internal vibration. we do not know what exactly vibrates
;; so we set the amplitude to one
(defn internal-swing [nu_one]
  (fn [t] (sin (* 2 'pi nu_one t))))

;; the particle is at position x = v * t (yes, even in special relativity)
;; so the time can be measured by the position of the particle
(defn t [x v] (/ x v))

;; calculate the phase of the internal swing at particle point x
(def internal-phase
  (asin ((internal-swing 'nu_one) (t 'x 'v))))

(is-equal! (* 2 'pi 'nu_one (/ 'x 'v))
           internal-phase)

;; ## A general Wave
;; now for soemthing completely different: general definition of a wave
(defn wave [omega k]
  (fn [x t] (sin (- (* omega t) (* k x)))))

;; with the usual definition of omega
(defn omega [nu] (* 2 'pi nu))

;; and the simplest possible definition for the wave-vector k:
;; a dispersion free wave traveling at phase-velocity V
(defn k [omega V] (/ omega V))

;; calculate the phase of the wave at point x and time t = x/u
;; note that u is an arbitrary velocity, used to set the time t

(def wave-phase
  (let [omega (omega 'nu)
        k     (k omega 'V)]
    (asin ((wave omega k) 'x (t 'x 'u)))))

(is-equal! (* 2 'pi 'nu (- (/ 'x 'u) (/ 'x 'V)))
           wave-phase)

;; ## Phase difference
;; since we can calculate the phase of both the internal vibration and the wave,
;; we can also calculate their difference.

;; calculate the phase difference at time t = x / v of the particle.
;; as a function of the ratio of the frequencies
(defn phase [x] (/ (asin x) (* 2 'pi)))

(defn phase-difference [r]
  (let [nu_one (* r 'nu)
        omega  (omega 'nu)
        k      (k omega 'V)]
    (- (phase ((internal-swing nu_one) (t 'x 'v)))
       (phase ((wave omega k) 'x (t 'x 'v))))))

(is-equal! (* 'nu (+ (* (- 'r 1) (/ 'x 'v)) (/ 'x 'V)))
           (phase-difference 'r))

;; show that internal-vibration and wave are in phase
;; when the ratio of the frequencies equals
;; a certain simple relation between
;; the velocity of the particle and the phase velocity
(defn nu-ratio-in-phase [v V] (- 1 (/ v V)))

(simplify (phase-difference (nu-ratio-in-phase 'v 'V)))

;; the Energy of the particle for the observer

(defn E [E0 gamma] (* E0 gamma))

;; we assume the wave has the frequency: energy devided by Planck's constant.
;; reminder: this relation holds in every frame of reference,
;; especially for the observer who is not in the rest frame.
(defn nu [E] (/ E 'h))

;; now that nu is set, calculate the general ratio of the frequencies
(defn nu-ratio [beta]
  (simplify (/ (nu_one (nu_naught 'E0) (gamma beta))
               (nu (E 'E0 (gamma beta))))))

;; this sets the value of the phase-velocity V
(defn V [v c] (/ (* c c) v))

(is-equal! (nu-ratio 'beta)
           (nu-ratio-in-phase (v 'beta 'c) (V (v 'beta 'c) 'c)))

;;note: the phase-velocity is always greater than the speed of light. It is independent
;; of the position x and the mass of the particle

;; the relativistic momentum is defined as
(defn p [m v gamma]
  (* m v gamma))

;; calculate the deBroglie wavelength (by dividing the phase-velocity by the frequency)
;; and show that it indeed is h devided by the momentum
(def de-broglie-wavelength
  (/ (V 'v 'c)
     (nu (E (E0 'm 'c) 'gamma))))

(is-equal! (/ 'h (p 'm 'v 'gamma))
           de-broglie-wavelength)

(println "Success!!")
