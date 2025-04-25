(ns cljthreetiles
  (:require [cheshire.core :as json]
            [clojure.string :as s]
            [twotiles :as tt]
            [hiccup2.core :as h]
            [scicloj.kindly.v4.api :as kindly]
            [scicloj.kindly.v4.kind :as kind]))

(run! #(ns-unmap *ns* %) (keys (ns-interns *ns*)))

^:kindly/hide-code
(def md
  (comp kindly/hide-code kind/md))

^:kindly/hide-code
(do
  (defn hiccdiv [parser opts n code]
    [:div
     [:span (h/raw "\n")]
     [:div {:id    (str "blocklyDiv" n)
            :style {:height (or (:height opts) "100px")}}]
     [:script (h/raw "
 var workspace" n " = Blockly.inject('blocklyDiv" n
                     "', {'toolbox': twotiles.toolbox, 'sounds': false});"
                     (if parser
                       (str "var xs" n " = " parser "('" code "');")
                       (str "var xs" n " = '" code "';"))
                     "const xmlDom" n " = Blockly.utils.xml.textToDom(xs" n ")
 Blockly.Xml.clearWorkspaceAndLoadFromXml(xmlDom" n ",workspace" n ")
")]
     (when (:xml opts)
       [:textarea {:style {:width "100%"}} (str code)])])

  (defn tiles-html
    ([parser code] (tiles-html parser nil code))
    ([parser opts code]
     (->> code
          (hiccdiv parser opts (s/replace (str (random-uuid)) "-" ""))
          h/html
          str)))

  :tiles-html-definition)

(md "# Definition of `clj-tiles` graphical blocks")

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

(def js-twotiles
  {:parse_clj (slurp "notebooks/twotiles.clj")
   :toolbox   toolbox
   :blocks    tiles-blocks})

(spit "docs/twotiles.js" (str "var twotiles = " (json/generate-string js-twotiles)))

(comment
  ;; "<script src=\"twotiles.clj\" type=\"application/x-scittle\"></script>"
  ;; only seems to work async despite
  ;; scittle.core.disable_auto_eval();scittle.core.eval_script_tags();
  ;; so I resort to this
  :end)

(kind/hiccup [:script {:src "twotiles.js"}])
(kind/scittle "")
(kind/hiccup [:script "var parse_clj = scittle.core.eval_string(twotiles.parse_clj);"])
(kind/hiccup [:script {:src "https://unpkg.com/blockly/blockly_compressed.js"}])
(kind/hiccup [:script "Blockly.defineBlocksWithJsonArray(twotiles.blocks);"])

(->> '(+ 1 2) (tiles-html "parse_clj") kind/html)

(def code '(->> (pow x 4)
                (for [x [1 2 3]])))

(kind/html
  (tiles-html "parse_clj"
              {:height "150px"
               :xml    true}
              (list :tiles/vert code)))

(md "# Without Scittle: write Blocks as XML into the file `mytiles.html`")

(defn hmap-indexed [hicc]
  (into [:div] (map-indexed (fn [i xml] (hiccdiv nil {:xml true} i xml)) hicc)))

(defn pagen [code-vec]
  [:html
   [:script {:src "docs/twotiles.js"}]
   [:script {:src "https://unpkg.com/blockly/blockly_compressed.js"}]
   [:script "Blockly.defineBlocksWithJsonArray(twotiles.blocks);"]
   [:script (h/raw "var dummy = " (json/generate-string {:dummy "⋮"}) ";")] ;;to get the dots right
   (hmap-indexed (map #(tt/twotiles-xml (pr-str %)) code-vec))])

(def code-vec
  [[1]
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
   '(f 11)
   ])

(spit "mytiles.html"
      (str (h/html (h/raw "<!DOCTYPE html>")
                   (pagen code-vec))))

;; 25.4. 11:30 - 14:15 2:45
;; 24.4. 18:30 - 20:45 2:15
;; 23.4. 10:30 - 14:15 3:45
;; 22.4. 18:00 - 22:00 4:00
;; 21.4. 12:00 - 21:00 9:00
;; 20.4. 15:20 - 17:50 2:30
;; 20.4. 14:15 - 14:55 :40
;; 20.4. 13:15 - 14:15 1:00
;; 20.4. 12:05 - 12:50 :45
;; 20.4. 10:40 - 12:05 1:15
