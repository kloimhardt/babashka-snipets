^:kindly/hide-code
(comment
  ;; clj -Sdeps "{:deps {org.scicloj/clay {:mvn/version \"2-beta15\"} com.nextjournal/beholder {:mvn/version \"1.0.2\"}}}"
  (do
    (require '[scicloj.clay.v2.api :as clay])
    (require '[nextjournal.beholder :as beholder])
    (defn make! [_] (clay/make! {:source-path "babashka-snipets/lambert.clj"}))
    (def watcher (beholder/watch make! "babashka-snipets"))
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
;; Buch Quelle https://ia600909.us.archive.org/7/items/bub_gb_G5I_AAAAcAAJ/bub_gb_G5I_AAAAcAAJ.pdf

(md "Daten Seite 40")
(def data
  {:zolle [51.6 54 54.4 58.2 58.9 55.9 59.9 61.10 64.2 67.3 58.5 73.0 54.0]
   :grade [1000 1049 1055 1129 1141 1082 1160 1201 1246 1306 1134 1417 1049]
   :C     ["frierendes Wasser"
           "Im Keller der Pariser Sternwarte"
           "Sternwarte2"
           "Wärem der Hand in mässiger Sommerluft, von verschiedenen Personen"
           "Handwärme 2"
           "Wärme der Luft, zur Zeit, da diese Wärme der Hände beobachtet worden"
           "schmelzende Butter"
           "schmelzend Unschlit"
           "gerinnend Wachs"
           "ein 10 bis 12 Gran schweres Stückchen Wachses ist ganz geschmolzen"
           "temperiert scheinendes Wasser"
           "siedendes Wasser"
           "temperierte Luft"]})

(def desc {:zolle "Zolle des Amontonschen Thermometers"
           :grade "Grade das Luftthermometers"})
(defn transpose [m]
  (apply mapv vector m))

^:kindly/hide-code
(defn tenpow [n]
  (apply * (repeat n 10)))

^:kindly/hide-code
(defn round [x dec]
  (double (/ (math/round (* x (tenpow dec))) (tenpow dec))))


(def tb1 (transpose ((juxt :zolle :grade :C) data)))

(defn luft->celsius [g siedepunkt]
  (double (/ (- g 1000) (/ (- siedepunkt 1000) 100))))

(defn tb2 [sp]
  (mapv (fn[[z g d]] [z g (luft->celsius g sp) d]) tb1))

;;Tabelle von Amonton lt. Buch und Umrechnung nach Celsius. Umrechnung leicht da Gefrier- und Siedepunkt von Wasser angegeben.

(tb2 1417)

(defn ordinate [xs ys]
  (let [n    (count xs)
        s_x  (reduce + xs)
        s_y  (reduce + ys)
        xy   (map #(* %1 %2) xs ys)
        s_xy (reduce + xy)
        x2   (map #(* % %) xs)
        s_x2 (reduce + x2)
        sx_2 (* s_x s_x)]
    (/ (- (* s_y s_x2) (* s_x s_xy))
       (- (* n s_x2) sx_2))))

(apply ordinate (map (transpose (tb2 1417)) [0 2]))

(md "Seite 47, § 89, Siedepunkt Wasser 1370 bestimmt")
(md "Ich werde als eine runde Zahl C = 1370 annehmen. Amontons fand 1417")

(mapv #(round % 2)
      (map #(apply ordinate
                   (map (transpose (tb2 %)) [0 2]))
           [1354 1370 1375]))

(md "1370 ist also rund -272 °C")
