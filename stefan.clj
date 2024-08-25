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
(ns stefan
  (:require [scicloj.kindly.v4.api :as kindly]
            [scicloj.kindly.v4.kind :as kind]
            [clojure.math :as math]))

^:kindly/hide-code
(def md
  (comp kindly/hide-code kind/md))

(defn postfix? [e]
  (#{"stel"}
    (str (last e))))

(defn infix? [e]
  (#{"plus" "minus" "mal" "von" "dot" "durch" "hoch" "Ein" "Milliard"
     "Komma"}
    (str (second e))))

(defn npow [x n]
  (apply * (repeat n x)))

(defn mypow [x n]
  (if (integer? n)
    (npow x n)
    (clojure.math/pow x n)))

(def plus +)
(def minus -)
(def mal *)
(def von *)
(def dot *)
(def durch /)
(def hoch mypow)
(def null 0)
(def Ein 1)
(def e (clojure.math/exp 1))
(def Milliard (fn [x] (* x (mypow 10 9))))
(def stel (fn [x y] (/ x y)))
(defn Komma [& lst]
  (Double/parseDouble (apply str (conj (rest lst) "." (first lst)))))


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

(defn bx [ex]
  (str "\\fbox{" (apply str (interpose " " ex)) "}"))

(defn bx2 [e]
  (if (coll? e)
    (if (= (str (second e)) "dot")
      (bx (conj (rest (rest e)) (first e)))
      (bx e))
    e))

(defn bx3 [e]
  (clojure.walk/postwalk bx2 e))

(defmacro calcbox [e & dbg]
  (let [swe (sw3 e)]
    {:code `'~swe
     :calc (if (seq dbg) (str dbg) swe)
     :tex  (bx3 e)}))

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
(defn round [x dec]
  (double (/ (math/round (* x (mypow 10 dec))) (mypow 10 dec))))

(defn dp-formel [X]
  (calcbox (2 mal ((e hoch ((0 Komma 0 0 767) dot X)) minus 1))))

;; X ist die Temperatur in Grad Celsius. Man muss wissen wo das X steh, welche Werte es ungefähr haben kann und wie man die Formel von innen nach aussen lesen muss, vom X her.

;; eigentlich 2.02 statt genau 2, aber auch mit 2.02 komm ich nur auf ein zwei zehntel hin.
;; 0.00767 ist ln(1.0077), weil formel im paper ist 1,0077^T, habs auf exp umgeändert

(kind/tex (:tex (dp-formel 0)))

(clojure.math/log 1.0077)

e

;; Unten die Werte in Tabellenform, die berechnete Differenz D&P-Stefan passt mit Paper überein, meine Nachrechnung D&P passt nicht ganz.

(def tb2 (map (fn [[T m t]]
                [T m (round (- t m) 2) t (round (:calc (dp-formel T)) 2)])
              (transpose [temper dp-meas dp-theor])))

;; Temperatur Messwert Differenz RechenWert_D&P Meine-Rechnung
tb2

(defn stefan-formel [X]
  (calcbox ((Ein (6 Milliard) stel)
            von
            (((X plus 273) hoch 4 )
             minus
             (273 hoch 4)))))

(kind/tex (:tex (stefan-formel 0)))
(kind/md "So eine Formel kann man immer auf zwei Arten lesen. Einml ganz normal. Probiers. Ein sechs Milliardstel von X plus 273 hoch 4 minus 273 hoch vier. Lesen kann jeder.")

(kind/md "Man sollte die Formel aber im zweiten Schritt immer auch vom X her lesen. Von innen heraus quasi. X plus 273, das ganze hoch 4, dann das ganze minus ca. 5 1/2 Milliarden und das ganze dann durch 6 Milliarden.")

(kind/md "Ich könnt mir vorstellen, dass diese Milliarden auch Dulong nd Petit abgeschreckt haben")

(def pow_273_4 (calcbox (273 mal (273 mal (273 mal 273)))))

(kind/tex (:tex pow_273_4))

(:calc pow_273_4)

(kind/md (str "Um genau zu sein, 273 hoch 4 ist folgende Zahl: " (mypow 273 4) ". Das musste man alles mit der Hand rechnen, sodass es wohl kein Wunder ist, dass es bis zu dem T^4 Gesetz so lange gedauert hat. D&P hat zwar Kommas, aber die Grössenordnungen sind im menschlichen Bereich. In der Moderne nicht mehr möglich weil Loschmidtzahl so hoch"))

(kind/md "drum sagt er auch -genauso einfache Formel wie D&P- denn die Milliarden machen's nicht unbedingt einfacher. Heutzutage sagen wir ein einfaches T^4 Gesetz")

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
