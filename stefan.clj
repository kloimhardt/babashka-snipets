^:kindly/hide-code
(comment
  ;; clj -Sdeps "{:deps {org.scicloj/clay {:mvn/version \"2-beta15\"} com.nextjournal/beholder {:mvn/version \"1.0.2\"}}}"
  ;; clj -M babashka-snipets/stefan.clj
  (do
    (require '[scicloj.clay.v2.api :as clay])
    (require '[nextjournal.beholder :as beholder])
    (defn make! [_] (clay/make! {:format      [:html] #_ [:quarto :pdf]
                                 :source-path "babashka-snipets/stefan.clj"}))
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

(md "# Code Präambel (bitte runterscrollen zum Hauptteil)")

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
    (clojure.math/pow x n)))

(defn transpose [m]
  (apply mapv vector m))

(defn round [x dec]
  (double (/ (math/round (* x (mypow 10 dec))) (mypow 10 dec))))

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
  (Double/parseDouble (apply str (conj (rest lst) "." (first lst)))))

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
           {:e  clojure.math/E
            :pi clojure.math/PI}})

(defmacro calcbox [ex & dbg]
  (let [swe (clojure.walk/postwalk #(sw gctx %) (threadzero gctx ex))
        bxe (if (vector? ex)
              (->> (cons (first ex) (apply concat (rest ex)))
                   (map (fn [mex] (clojure.walk/postwalk #(bx gctx %) mex)))
                   (interpose " \\\\ ")
                   (apply str))
              (clojure.walk/postwalk #(bx gctx %) ex))
        hxe (if (vector? ex)
              (->> (cons (first ex) (apply concat (rest ex)))
                   (map (fn [mex] (clojure.walk/postwalk #(hx gctx %) mex)))
                   (map #(vector :div %))
                   (into [:div]))
              (clojure.walk/postwalk #(hx gctx %) ex))]
    {:code `'~swe
     :calc (if (seq dbg) (into [] dbg) swe)
     :tex  bxe
     :hiccup hxe}))

(md "# J. Stefan: Über die Beziehung zwischen der Wärmestrahlung und der Temperatur")

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

;; X ist die Temperatur in Grad Celsius. Man muss wissen wo das X steh, welche Werte es ungefähr haben kann und wie man die Formel von innen nach aussen lesen muss, vom X her.

;; eigentlich fittet 2.02 besser statt genau 2 (im Paper selbst bisher keinen Wert gefunden), aber auch mit 2.02 komm ich nur auf ein zwei zehntel hin.
;; 0.00767 ist ln(1.0077), weil formel im paper ist 1,0077^T, habs auf exp umgeändert

(kind/tex (:tex (dp-formel 0)))

(kind/hiccup (:hiccup (dp-formel 0)))

(dp-formel 0)

(clojure.math/log 1.0077)

(def Schulwissen (-> gctx :Schulwissen))
(:e Schulwissen)

;; Unten die Werte in Tabellenform, die berechnete Differenz D&P-Stefan passt mit Paper überein, meine Nachrechnung D&P passt nicht ganz.

(def tb2 (map (fn [[T m t]]
                [T m (round (- t m) 2) t (round (:calc (dp-formel T)) 2)])
              (transpose [temper dp-meas dp-theor])))

;; Temperatur Messwert Differenz RechenWert_D&P Meine-Rechnung
tb2

(defn stefan-formel [Celsius]
  (calcbox [(((((T plus x) hoch 4 )
               minus
               (T hoch 4))
              mal
              B)
             mit
             [x in Celsius])
            [und [(Ein (6 Milliard) stel) für B]]
            [und [273 für T]]]))

(kind/tex (:tex (stefan-formel 0)))

(md "So eine Formel kann man immer auf zwei Arten lesen. Einml ganz normal. Probiers. Ein sechs Milliardstel von X plus 273 hoch 4 minus 273 hoch vier. Lesen kann jeder.")

(md "Man sollte die Formel aber im zweiten Schritt immer auch vom X her lesen. Von innen heraus quasi. X plus 273, das ganze hoch 4, dann das ganze minus ca. 5 1/2 Milliarden und das ganze dann durch 6 Milliarden.")

(md "Ich könnt mir vorstellen, dass diese Milliarden auch Dulong nd Petit abgeschreckt haben")

(def pow_273_4 (calcbox [(((T mal T) mal T) mal T) [mit [T gleich 273]]]))

(kind/tex (:tex pow_273_4))

(:calc pow_273_4)

(md (str "Um genau zu sein, 273 hoch 4 ist folgende Zahl: " (mypow 273 4) ". Das musste man alles mit der Hand rechnen, sodass es wohl kein Wunder ist, dass es bis zu dem T^4 Gesetz so lange gedauert hat. D&P hat zwar Kommas, aber die Grössenordnungen sind im menschlichen Bereich. In der Moderne nicht mehr möglich weil Loschmidtzahl so hoch"))

(md "drum sagt er auch -genauso einfache Formel wie D&P- denn die Milliarden machen's nicht unbedingt einfacher. Heutzutage sagen wir ein einfaches T^4 Gesetz")

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

(md "# Kleine Fingerübungen")

(def bsp0 (calcbox (1 plus 3)))
(kind/tex (:tex bsp0))
(:calc bsp0)
;; praktisch zum debuggen: der generierte code
(:code bsp0)

(def bsp1 (calcbox (2 plus 1) "andere Rechnung" (+ 4 5)))
(kind/tex (:tex bsp1))
;; noch praktischer zum debuggen: calculation unterdrücken
(:calc bsp1)

(def bsp2 (calcbox ((X plus 1) mit [X gleich [(Y plus 2) mit [Y gle=ich 3]]])))
(kind/tex (:tex bsp2))
(:calc bsp2)

(def bsp3 (calcbox ( [(X plus Y) mit [X ist-gleich 3]] mit [Y ist 2]) ))
;; es ist wurst ob glei=ch, ist-gleich, -in-: nur ein Füllwort
(kind/tex (:tex bsp3))
(:calc bsp3)

(defn bsp4 [Sekunden Y]
  (calcbox ((X plus Y) mit [X -in- Sekunden])))

(kind/tex (:tex (bsp4 0 0)))
(:calc (bsp4 5 6))

(defn bsp5 [{:keys [Schulwissen]}]
  (calcbox ((e hoch pi) mit [[e pi] aus Schulwissen])))
(kind/tex (:tex (bsp5 gctx)))
(bsp5 gctx)

(def bsp6 (calcbox [(X plus Y) [mit [X gleich (Y plus 7)]] [mit [Y gle=ich 3]]]))
(kind/tex (:tex bsp6))
bsp6
