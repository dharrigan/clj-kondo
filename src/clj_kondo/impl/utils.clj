(ns clj-kondo.impl.utils
  {:no-doc true}
  (:require
   [clojure.walk :refer [prewalk]]
   [rewrite-clj.node.protocols :as node]
   [rewrite-clj.node.whitespace :refer [whitespace?]]
   [clj-kondo.impl.parser :as p]
   [clojure.string :as str]))

(defn comment? [node]
  (= 'comment (first node)))

(defmacro some-call
  "Determines if expr is a call to some symbol. Returns symbol if so."
  [expr & syms]
  (let [syms (set syms)]
    `(and (list?  ~expr)
          ((quote ~syms) (first ~expr)))))

(defn remove-noise
  [expr] expr)

(defn node->line [filename node level type message]
  (let [m (meta node)]
    (prn "meta" m)
    {:type type
     :message message
     :level level
     :row (:line m)
     :col (:column m)
     :filename filename}))

(defn parse-string-all
  ([s] (p/parse-string s))
  ([s config]
   (p/parse-string s)))

(defn parse-string [s]
  (first (parse-string-all s)))

(defn filter-children
  "Recursively filters children by pred"
  [pred children]
  (mapcat #(if (pred %)
             [%]
             (if-let [cchildren (:children %)]
               (filter-children pred cchildren)
               []))
          children))

;;;; Scratch

(comment
  (some-call (parse-string "(comment 1 2 3)") comment)

  )
