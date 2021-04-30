(ns user
  (:require [juxt.clip.repl :refer [start stop set-init! system]]
            [clojure.tools.namespace.repl :refer [set-refresh-dirs]]))

(defn reset
  []
  (clojure.tools.namespace.repl/set-refresh-dirs "dev/src" "src" "test")
  (set-init! (fn []))
  (juxt.clip.repl/reset))