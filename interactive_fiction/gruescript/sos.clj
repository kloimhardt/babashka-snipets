(ns sos
  (:require [org.httpkit.server :as srv]
            [clojure.string :as str]))

(def the-party-line-html (slurp "The_Party_Line.html"))
(def the-party-line-html-header
  (get (str/split the-party-line-html #"</body>") 0))

(defn compile-game [gru-src]
  (let [b (str "\n</body>\n<textarea id=\"gsEdit\" style=\"display: none;\">")
        d "\n</textarea>\n</html>"]
    (str the-party-line-html-header b gru-src d)))

(defn make-http-message [gru-filename]
  {:body   (compile-game (slurp gru-filename))
   :status 200})

(defn the-party-line [_]
  (make-http-message "The_Party_Line.gru"))

(defn cloak-of-darkness [_]
  (make-http-message "Cloak_of_Darkness.gru"))

(defn run-server []
  (srv/run-server #'cloak-of-darkness {:port 8888})
  (println "Server started at http://localhost:8888")
  @(promise))

(run-server)
