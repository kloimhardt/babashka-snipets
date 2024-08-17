^:kindly/hide-code
(comment
  ;; clj -Sdeps "{:deps {org.scicloj/clay {:mvn/version \"2-beta15\"} com.nextjournal/beholder {:mvn/version \"1.0.2\"}}}"
  (do
    (require '[scicloj.clay.v2.api :as clay])
    (require '[nextjournal.beholder :as beholder])
    (defn make! [_] (clay/make! {:source-path "babashka-snipets/stefan.clj"}))
    (def watcher (beholder/watch make! "babashka-snipets/notebooks"))
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

(defn mypow [x n]
  (apply * (repeat n x)))

;; Unten passt die Differenz mit Paper überein, auch Meine-Rechnung lt. Paper gerechnet passt
(def tb7 (map (fn [[T m st-t]]
                [T m (round (- st-t m) 2) st-t
                 (round (/
                          (- (mypow (+ 273 T) 4) (mypow 273 4))
                          (* 6 (tenpow 9)))
                        2)])
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
