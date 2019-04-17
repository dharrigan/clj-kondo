(ns clj-kondo.impl.parser
  (:require [clojure.tools.reader :as r]
            [clojure.tools.reader.edn :as er]
            [clojure.tools.reader.reader-types :as rt])
  (:import [java.io StringReader PushbackReader]))

(set! *warn-on-reflection* true)

(defn try-read
  [^clojure.tools.reader.reader_types.IndexingPushbackReader in eof]
  (let [;; r (r/read in false eof)
        r (er/read in false eof nil)]
    (prn "READ" r)
    r)
  #_(try (let [expr (binding [*read-eval* false] (r/read in false eof))]
           expr)
         (catch Throwable e
           (println "could not read" (.getMessage e)))))

(defn parse-string
  [s]
  (let [eof (Object.)
        reader (rt/source-logging-push-back-reader
                (rt/string-push-back-reader s))
        exprs (with-open [reader reader]
                (loop [exprs []]
                  (let [next-expr (try-read reader eof)]
                    (cond
                      (= eof next-expr) exprs
                      :else (recur (conj exprs next-expr))))))]
    exprs))

;;;;

(comment
  (map meta (parse-string "^:private (foo 1) (bar 2)"))
  (parse-string (slurp "src/clj_kondo/impl/parser.clj"))
  (parse-string (slurp "src/clj_kondo/impl/parser.clj"))

  (-> (r/read-string "(defn ^{:private true} [^String x] x)") second first meta)
  (-> (r/read-string "") second first meta)
  (prn  #::{:b 1})
  (require '[clojure.tools.namespace.parse :as ns-parse])
  (ns-parse/read-ns-decl (java.io.PushbackReader. (StringReader. "(ns foo (:require [bar]))")))
  ;; we can look inside this code to see how we can parse nested libspecs
  (ns-parse/deps-from-ns-decl '(ns foo (:require [bar
                                                  [baz]
                                                  [foo :as f]])))
  (parse-string "(comment 1 2 3)")
  (-> (parse-string "(defmacro foo [x] `(1 2 x ~x))") first (nth 3) meta)
  (-> (parse-string "(defn foo
                       [x]
                       x)") first (nth 3) meta)
  (parse-string "(cond 1 2)")
  (meta (first (parse-string "{:a 1 :b ##Inf}")))
  (try (parse-string "{:a 1 :a ##Inf}")
       (catch Exception e (ex-data e)))

  
  )
