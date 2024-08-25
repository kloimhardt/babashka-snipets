^:kindly/hide-code
(comment
  ;; clj -Sdeps "{:deps {org.scicloj/clay {:mvn/version \"2-beta15\"} com.nextjournal/beholder {:mvn/version \"1.0.2\"}}}"
  (do
    (require '[scicloj.clay.v2.api :as clay])
    (require '[nextjournal.beholder :as beholder])
    (defn make! [_] (clay/make! {:source-path "babashka-snipets/stefan.clj"}))
    (def watcher (beholder/watch make! "babashka-snipets"))
    (make! nil)
    )
  )

^:kindly/hide-code
(comment
  ;; clj -Sdeps "{:deps {org.scicloj/clay {:mvn/version \"2-beta15\"}}}"
  (do
    (require '[scicloj.clay.v2.api :as clay])
    (clay/make! {:source-path "babashka-snipets/stefan.clj"})
    )
  )

^:kindly/hide-code
(ns stefan
  (:require [scicloj.kindly.v4.api :as kindly]
            [scicloj.kindly.v4.kind :as kind]
            [clojure.math :as math]))

^:kindly/hide-code
(def md
  (comp kindly/hide-code kind/md))

(defn postfix? [e]
  (= (str (last e)) "stel"))

(defn infix? [e]
  true)

(defn sw [e]
  (cond
    (postfix? e)
    (conj (butlast e) (last e))
    (infix? e)
    (conj (rest (rest e)) (first e) (second e))
    :else e))

(defn sw2 [e]
  (if (coll? e) (sw e) e))

(defn sw3 [e]
  (clojure.walk/postwalk sw2 e))

(defn bx [e]
  (str "\\fbox{" (apply str (interpose " " e)) "}"))

(defn bx2 [e]
  (if (coll? e) (bx e) e))

(defn bx3 [e]
  (clojure.walk/postwalk bx2 e))

(defmacro calcbox [e & dbg]
  (let [swe (sw3 e)]
       {:code `'~swe
        :calc (if (seq dbg) (str dbg) swe)
        :tex (bx3 e)}))

(defn mypow [x n]
  (apply * (repeat n x)))

(def hoch mypow)
(def plus +)
(def minus -)
(def mal *)
(def durch /)

(md "# Start now")

;;Temperatur in Celsius
(def temper [80 100 120 140 160 180 200 220 240])

;; Dulong und Petit angegebene Messdaten (extrapoliert aus Messungen)
(def dp-meas [1.74 2.30 3.02 3.88 4.89 6.10 7.40 8.81 10.69])

;; von D&P angegeben als nach ihrer Heuristik gerechnet
(def dp-theor [1.72 2.33 3.05 3.89 4.87 6.03 7.34 8.89 10.68])

;; vom Stefan berechnete Werte
(def st-theor [1.66 2.30 3.05 3.92 4.93 6.09 7.42 8.92 10.62])

^:kindly/hide-code
(defn transpose [m]
  (apply mapv vector m))

^:kindly/hide-code
(defn tenpow [n]
  (apply * (repeat n 10)))

^:kindly/hide-code
(defn round [x dec]
  (double (/ (math/round (* x (tenpow dec))) (tenpow dec))))

;; Unten die Werte in Tabellenform, die berechnete Differenz passt mit Stefan-Paper überein
(def tb2 (map (fn [[T m t]]
                [T m (round (- t m) 2) t])
              (transpose [temper dp-meas dp-theor])))

;; Temperatur Messwert Differenz RechenWert_D&P
tb2

(def Ein 1)
(def Milliard (fn [x] (* x (tenpow 9))))
(def stel (fn [x y] (/ x y)))
(def von *)

(defn stefan-formel [X]
  (calcbox ((Ein (6 Milliard) stel)
            von
            (((X plus 273) hoch 4 )
             minus
             (273 hoch 4)))))

(kind/tex (:tex (stefan-formel 0)))
(kind/md "So eine Formel kann man immer auf zwei Arten lesen. Einml ganz normal. Probiers. Ein sechs Milliardstel von X plus 273 hoch 4 minus 273 hoch vier. Lesen kann jeder.")

(mypow 273 4)

(kind/md "Man sollte die Formel aber im zweiten Schritt immer auch vom X her lesen. Von innen heraus quasi. X plus 273, das ganze hoch 4, dann das ganze minus ca. 5 1/2 Milliarden und das ganze dann durch 6 Milliarden.")

(kind/md "Ich könnt mir vorstellen, dass diese Milliarden auch Dulong nd Petit abgeschreckt haben")

(:code (stefan-formel 0))

;; Unten passt die Differenz mit Paper überein, auch Meine-Rechnung lt. Paper gerechnet passt
(def tb7 (map (fn [[T m st-t]]
                [T m (round (- st-t m) 2) st-t
                 (round (:calc (stefan-formel T)) 2)])
              (transpose [temper dp-meas st-theor])))

;; Temperatur Messwert Differenz Rechenwert_St Meine-Rechnung
tb7

(defn minsec [sec]
  (mapv math/round [(quot sec 60) (mod sec 60)]))

(defn speed->sec [sp]
  (* (/ 20.0 sp) 60))

^:kindly/hide-code
(defn r-transpose [x]
  (reverse (transpose x)))

(defn tb3 [tbl]
  (map (fn [tb]
         (map #(minsec (speed->sec %)) tb))
    tbl))

;; Umrechnung von Messwerten und Rechnungen nach Zeitdauer -- MinutenSekunden, Absteigende Temperatur

(tb3 (r-transpose [dp-meas dp-theor st-theor]))

(def Grad 'Grad)
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


(def minus 'minus)
(def plus 'plus)

(defn tb6 [tb]
  (mapv (fn [[[am as] [bm bs]]]
          [[bs
            (if (neg? (- as bs)) minus plus)
            (abs (- as bs))]
           ist
           as])
        tb))
;; Differenz der Sekunden: D&P-Rechnung Differenz D&P-Messung
(tb6 (tb3 (r-transpose [dp-meas dp-theor])))

;; Differenz der Sekunden: St-Rechnung Differenz D&P-Messung
(tb6 (tb3 (r-transpose [dp-meas st-theor])))
