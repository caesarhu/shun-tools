(ns caesarhu.shun-tools.shun-tools)

(defn flatten-1
  [xs]
  (let [all-seq? #(every? sequential? %)]
    (filter (complement all-seq?)
            (tree-seq all-seq? seq xs))))