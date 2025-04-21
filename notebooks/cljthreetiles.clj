(ns cljthreetiles
  (:require [hiccup2.core :as h]
            [cheshire.core :as json]
            [scicloj.kindly.v4.api :as kindly]
            [scicloj.kindly.v4.kind :as kind]
            [clojure.string :as s]))

^:kindly/hide-code
(def md
  (comp kindly/hide-code kind/md))

^:kindly/hide-code
(do

  (defmulti gen (fn [m _] (:type m)))

  (defmethod gen :slot [] nil)

  (defn blockmap [type givenid & [inline?]]
    (cond-> {:type type :id (str type givenid)}
      (some? inline?) (assoc :inline (str inline?))))

  (defmethod gen :num [{:keys [nummer]} givenid]
    [:block (blockmap "num" givenid)
     [:field {:name "nummer"} nummer]])

  (defmethod gen :text [{:keys [dertext]} givenid]
    [:block (blockmap "text" givenid)
     [:field {:name "dertext"} dertext]])

  ;; is really a vector
  (defmethod gen :args [{:keys [argsvec inline?]} givenid]
    (let [xml-block-type      (str "args-" (count argsvec))
          {:keys [id] :as bm} (blockmap xml-block-type givenid inline?)]
      (into [:block bm]
            (map-indexed (fn [idx v]
                           [:value {:name (str "arg_" (+ idx 1))}
                            (gen v (str (+ idx 1) "-" id))]) argsvec))))

  (defmethod gen :list [{:keys [argsvec inline?]} givenid]
    (let [xml-block-type      (str "list-h-" (count argsvec))
          {:keys [id] :as bm} (blockmap xml-block-type givenid inline?)]
      (into [:block bm]
            (map-indexed (fn [idx v]
                           [:value {:name (str "args-" (+ idx 1))}
                            (gen v (str (+ idx 1) "-" id))]) argsvec))))


  (defmethod gen :fun [{:keys [kopf argsvec subtype inline?]} givenid]
    (let [xml-block-type      (str subtype "-" (inc (count argsvec)) "-inp")
          {:keys [id] :as bm} (blockmap xml-block-type givenid inline?)]
      (into [:block bm
             [:field {:name "kopf"} kopf]]
            (map-indexed (fn [idx v]
                           [:value {:name (str "args-" (+ idx 2))}
                            (gen v (str (+ idx 2) "-" id))]) argsvec))))

  (defn rearrange [m]
    (let [a (partition 2 m)]
      (concat (map first a) (map second a))))

  (defmethod gen :map [{:keys [argsvec subtype inline?]} givenid]
    (let [xml-block-type      (str subtype "-" (* (count argsvec) 2) "-inp")
          {:keys [id] :as bm} (blockmap xml-block-type givenid inline?)]
      (into [:block bm]
            (rearrange ;;(rearrange [1 2 3 4]) => (1 3 2 4)
              ;;the Blockly ui does this rearrangemnt to the XML and
              ;;the end->code/parse depends on it
              ;;namely that in the XML all the :field are given first and
              ;;then the :value
              (apply concat
                     (map-indexed (fn [idx v]
                                    (let [i (inc (* idx 2))]
                                      [[:field {:name (str "key-" i)}
                                        (str (first v))]
                                       [:value {:name (str "val-" (inc i))}
                                        (gen (second v) (str (inc i) "-" id))]]))
                                  argsvec))))))


  (defn addcoords [block [x y]]
    (update block 1 #(-> %
                         (assoc :x x)
                         (assoc :y y))))

  (defn gen-str [[x y]]
    (str "-" x "-" y))

  (defn page [coords & blocks]
    (->> blocks
         (map-indexed (fn [idx blk] (addcoords (gen blk (gen-str (coords idx)))
                                               (coords idx))))
         (into [:xml])
         ;;html
         ))

  (def slot {:type :slot})

  (defn num [nummer]
    {:type :num :nummer nummer})

  (defn kw [k]
    {:type :num :nummer (str k)})

  (defn text [txt]
    {:type :text :dertext txt})

  (defn tiles-deref [e]
    {:type :num :nummer (str "@" e)})

  (defn fun [name & argsvec]
    {:type :fun :subtype "funs-h" :kopf name :argsvec argsvec})

  (defn fun-infi [name & argsvec]
    (assoc (apply fun name argsvec) :subtype "infi-h"))

  (defn args [& argsvec]
    {:type :args :argsvec argsvec})

  (defn lst [& argsvec]
    {:type :list :argsvec argsvec})

  (defn t-map [& argsvec]
    (if (> (count argsvec) 1)
      {:type :map :subtype "map-h" :argsvec argsvec}
      (text "clj-tiles error: one-entry map not allowed")))

  (defn chapter [& pages] (into [] pages))

  (defn exp [v]
    (if (vector? v)
      (let [erst (first v)
            appl (fn [fuct] (apply fuct erst (map exp (into [] (rest v)))))]
        (cond
          (and (= (count v) 3) (#{"/" "+" "*" "-"} erst)) (appl fun-infi)
          (#{"def" "defn" "do"} erst)                     (assoc (appl fun) :inline? false)
          :else                                           (appl fun)))
      (cond
        (map? v)    v
        (nil? v)    (num "nil")
        (string? v) (text v)
        :else       (num v))))

  (defn infix-sensible? [lst]
    (when (and (list? lst) (= (count lst) 3))
      (let [[f s t] lst]
        (and  (#{"/" "+" "*" "-"} (str f))
              (or (symbol? s) (number? s) (infix-sensible? s))
              (or (symbol? t) (number? t) (infix-sensible? t))))))

  (defn parse [l]
    (cond
      (list? l)
      (let [erst (str (first l))
            appl (fn [fuct] (apply fuct erst (map parse (rest l))))]
        (cond
          (or (list? (first l)) (= ":tiles/slot" erst)) (apply lst (map parse l))
          (= ":tiles/vert" erst)                        (assoc (parse (second l)) :inline? false)
          (= ":tiles/keep" erst)                        (parse (second l))
          (= ":tiles/num" erst)                         (num (second l))
          (= "clojure.core/deref" erst)                 (tiles-deref (second l))
          (= "quote" erst)                              (num (str "'" (second l)))
          ;;(infix-sensible? l) (appl fun-infi) ;;detect infix automatically for simple expressions
          (= ":tiles/infix" erst)                       (assoc (parse (second l)) :subtype "infi-h")
          (#{"def" "defn" "do"} erst)                   (assoc (appl fun) :inline? false)
          :else                                         (appl fun)))
      (vector? l)
      (if (empty? l)
        (num "[ ]")
        (apply args (map parse l)))
      (map? l)
      (cond
        (empty? l)
        (num "{ }")
        (:tiles/numslot l)
        slot
        :else
        (apply t-map (map (fn [[k v]] [k (parse v)]) l)))
      (and (set? l) (empty? l))
      (num "#{ }")
      (= :tiles/slot l) slot
      (nil? l)          (num "nil")
      (string? l)       (text l)
      (keyword? l)      (kw l)
      :else             (num l)))

  (defn shift-coords [nofblocks & coords]
    (->> (range 0 nofblocks)
         (mapv (fn [x] [0 (* 50 x)]))
         (concat coords)
         (mapv (fn [[x y]] [(+ x 10) (+ y 10)]))))

  (defn p-gen [parser-fn]
    (fn [coords & blocks]
      (let [shifted (apply shift-coords (count blocks) coords)]
        (->> blocks
             (map-indexed (fn [idx blk]
                            (addcoords (gen (parser-fn blk) (gen-str (shifted idx)))
                                       (shifted idx))))
             (into [:xml])
             ;;html
             ))))

  (def pg (p-gen exp))
  (def rpg (p-gen parse))
  :rpg-definition)

^:kindly/hide-code
(do
  (defn hiccdiv [n opts code-xml]
    ;; TODO: toolbox is hardcoded
    [:div
     [:span (h/raw "\n")]
     [:div {:id (str "blocklyDiv" n)
            :style {:height (or (:height opts) "100px")}}]
     [:script (h/raw "
 var workspace" n " = Blockly.inject('blocklyDiv" n
                     "', {'toolbox': toolbox, 'sounds': false});

 var xs" n " = '" code-xml "';
 const xmlDom" n " = Blockly.utils.xml.textToDom(xs" n ")
 Blockly.Xml.clearWorkspaceAndLoadFromXml(xmlDom" n ",workspace" n ")
")]
     (when (:xml opts)
       [:textarea {:style {:width "100%"}} code-xml])])

  (defn tiles-html
    ([code] (tiles-html code nil))
    ([code opts]
     (->> code
          (rpg [[0 0]])
          h/html
          str
          (hiccdiv (s/replace (str (random-uuid)) "-" "") opts)
          h/html
          str)))

  :tiles-html-definition)

(md "# Definition of `clj-tiles` graphical blocks")

(kind/html "<script src=\"https://unpkg.com/blockly/blockly_compressed.js\"></script>")

(defn hblock [type message color args]
  {:type         type
   :message0     message
   :args0        args
   :inputsInline true
   :output       nil
   :colour       color})

(defn hargs [type-name]
  (mapv (fn [[type name]] {:type type :name name}) type-name))

(defn input-values [prfx n m]
  (mapv (fn [i] ["input_value" (str prfx i)]) (range n (inc m))))

(def tiles-blocks
  ;; aus cljtiles/public/sicmblocks_fun.js
  [(hblock "num" "%1" "#A65C81"
           (hargs [["field_input" "nummer"]]))

   (hblock "text" "\" %1 \"" "#A65C81"
           (hargs [["field_input" "dertext"]]))

   (hblock "funs-h-2-inp" "%1 %2" 270
           (hargs [["field_input" "kopf"]
                   ["input_value" "args-2"]]))

   (hblock "funs-h-3-inp" "%1 %2 %3" 140
           (hargs (concat [["field_input" "kopf"]]
                          (input-values "args-" 2 3))))

   (hblock "funs-h-4-inp" "%1 %2 %3 %4" 230
           (hargs (concat [["field_input" "kopf"]]
                          (input-values "args-" 2 4))))

   (hblock "funs-h-5-inp" "%1 %2 %3 %4 %5" 360
           (hargs (concat [["field_input" "kopf"]]
                          (input-values "args-" 2 5))))

   (hblock "funs-h-6-inp" "%1 %2 %3 %4 %5 %6" "#A65C81"
           (hargs (concat [["field_input" "kopf"]]
                          (input-values "args-" 2 6))))

   (hblock "funs-h-7-inp" "%1 %2 %3 %4 %5 %6 %7" 140
           (hargs (concat [["field_input" "kopf"]]
                          (input-values "args-" 2 7))))

   (hblock "args-1" "%1" 270
           (hargs [["input_value" "arg_1"]]))

   (hblock "args-2" "%1 %2" 140
           (hargs (input-values "arg_" 1 2)))

   (hblock "args-3" "%1 %2 %3" 230
           (hargs (input-values "arg_" 1 3)))

   (hblock "args-4" "%1 %2 %3 %4" 360
           (hargs (input-values "arg_" 1 4)))

   (hblock "args-5" "%1 %2 %3 %4 %5" "#A65C81"
           (hargs (input-values "arg_" 1 5)))

   (hblock "args-6" "%1 %2 %3 %4 %5 %6" 140
           (hargs (input-values "arg_" 1 6)))

   (hblock "list-h-2" "%1 \u007C %2" 70
           (hargs (input-values "args-" 1 2)))

   (hblock "infi-h-3-inp" "%1 %2 %3" 140
           (hargs [["input_value" "args-2"]
                   ["field_input" "kopf"]
                   ["input_value" "args-3"]]))])

(def toolbox
  {:kind "categoryToolbox"
   :contents
   [{:kind "category"
     :name "⋮"
     :contents
     [{:kind "block" :type "num"}
      {:kind "block" :type "funs-h-2-inp"}
      {:kind "block" :type "funs-h-3-inp"}]}]})

(md "# Blocks in the Clay workspace")

(kind/html
  (str "<script>"
       "var toolbox = " (json/generate-string toolbox) ";"
       "var tiles_blocks  = " (json/generate-string tiles-blocks) ";"
       "Blockly.defineBlocksWithJsonArray(tiles_blocks);"
       "</script>"))

(-> '(+ 1 2) tiles-html kind/html)

(def code '(->> (pow x 4)
                (for [x [1 2 3]])))

(kind/html
  (tiles-html (list :tiles/vert code)
              {:height "150px"
               :xml    true}))

(md "# Write Blocks into the file `mytiles.html` ")

^:kindly/hide-code
(do
  (defn hmap-indexed [hicc]
    (into [:div] (map-indexed (fn [i xml] (hiccdiv i {:xml true} xml)) hicc)))

  (defn pagen [content]
    [:html
     [:script {:src "https://unpkg.com/blockly/blockly_compressed.js"}]
     [:script (h/raw "
var blocks = " (:blocks content) ";
Blockly.defineBlocksWithJsonArray(blocks);
")]
     [:script (h/raw "var toolbox = " (:toolbox content) ";")]
     (hmap-indexed (:code-xml content))])

  :pagen-definition)

(defn content [code-vec]
  {:blocks   (json/generate-string tiles-blocks)
   :toolbox  (json/generate-string toolbox)
   :code-xml (map (fn [code] (str (h/html (rpg [[0 0]] code)))) code-vec)})

(def code-vec
  ['(:tiles/vert (fun a))
   '((this-returns-a-function "three=") 3)
   ["a"]
   '(def (myfunction x base)
      (return (log x base)))
   '(define myfunction
      (parameters x
                  base)
      (return (log x base)))
   '(->> (pow x 4) (for [x [1 2 3]]))
   [1 2 3 4 5 6]
   [1 2 3 4 5]
   [1 2 3 4]
   [1 2 3]
   [1 2]
   [1]
   '(f 1 2 3 4 5 6)
   '(f 1 2 3 4 5)
   '(f 1 2 3 4)
   '(f 1 2 3)
   '(f 1 2)
   '(f 1)
   ])

(defonce write-html
  (str "only once"
       (spit "mytiles.html"
             (str (h/html (h/raw "<!DOCTYPE html>")
                          (pagen (content code-vec)))))))

;; 21.4. 12:00 - 21:00 9:00
;; 20.4. 15:20 - 17:50 2:30
;; 20.4. 14:15 - 14:55 :40
;; 20.4. 13:15 - 14:15 1:00
;; 20.4. 12:05 - 12:50 :45
;; 20.4. 10:40 - 12:05 1:15
