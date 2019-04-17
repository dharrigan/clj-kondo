(ns clj-kondo.impl.macroexpand
  {:no-doc true}
  (:require
   [clj-kondo.impl.utils :refer [some-call filter-children]]
   [clojure.walk :refer [prewalk]]
   [clj-kondo.impl.utils :refer [parse-string]]
   [rewrite-clj.node.protocols :as node :refer [tag]]
   [rewrite-clj.node.seq :refer [vector-node list-node]]
   [rewrite-clj.node.token :refer [token-node]]))

;;;; macro expand

(defn expand-> [expr]
  (let [children (rest expr)]
    (loop [[child1 child2 & children :as all-children] children]
      (if child2
        (if (list? child2)
          (recur
           (let [res (into
                      [(with-meta
                         (list (reduce into [[(first (rest child2))]
                                             [child1] (rest (rest child2))]))
                         #_(list-node (reduce into
                                            [[(first (:children child2))]
                                             [child1] (rest (:children child2))]))
                         (meta child2))] children)]
             res))
          (recur (into [(with-meta (list child2 child1) #_(list-node [child2 child1])
                          (meta child2))] children)))
        child1))))

#_(comment
  (macroexpand '(-> x inc inc inc))
  (meta (expand-> (parse-string "(-> x inc inc inc)")))
  (meta (macroexpand (parse-string "(-> x inc inc inc)")))
  )

(defn find-fn-args [children]
  (filter-children #(and (= :token (tag %))
                         (:string-value %)
                         (re-matches #"%\d?\d?" (:string-value %)))
                   children))

(defn expand-fn [{:keys [:children] :as expr}]
  (let [{:keys [:row :col] :as m} (meta expr)
        fn-body (with-meta (list-node children)
                  {:row row
                   :col (inc col)})
        args (find-fn-args children)
        arg-list (vector-node args)]
    (with-meta
      (list-node [(token-node 'fn*) arg-list fn-body])
      m)))

(defn expand-all [expr]
  expr
  #_(clojure.walk/prewalk
   #(if (:children %)
      (assoc % :children
             (map (fn [n]
                    (cond (some-call n ->)
                          (expand-> n)
                          (= :fn (node/tag n))
                          (expand-fn n)
                          :else n))
                  (:children %)))
      %)
   expr))

;;;; Scratch

(comment
  (parse-string "{:a 1 :a 2}") ;; this is a reason to stay with rewrite-clj for now
  )
