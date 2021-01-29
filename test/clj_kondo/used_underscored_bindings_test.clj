(ns clj-kondo.used-underscored-bindings-test
  (:require [clj-kondo.test-utils :refer [assert-submaps lint!]]
            [clojure.test :refer [deftest is testing]]
            [missing.test.assertions]))

(def used-underscored-binding-linter '{:linters {:used-underscored-binding {:level :warning}}})

(deftest used-underscored-bindings-test
  (assert-submaps
   '({:file "<stdin>", :row 1, :col 7, :level :warning, :message "used underscored binding _x"})
   (lint! "(let [_x 1])" used-underscored-binding-linter))
  (assert-submaps '({:file "<stdin>",
                     :row 1,
                     :col 8,
                     :level :warning,
                     :message "used underscored binding _x"}
                    {:file "<stdin>",
                     :row 1,
                     :col 13,
                     :level :warning,
                     :message "used underscored binding _y"})
   (lint! "(loop [_x 1 _y 2])" used-underscored-binding-linter))
  (assert-submaps
   '({:file "<stdin>", :row 1, :col 10, :level :warning, :message "used underscored binding _x"})
   (lint! "(if-let [_x 1] 1 2)" used-underscored-binding-linter))
  (assert-submaps
   '({:file "<stdin>", :row 1, :col 11, :level :warning, :message "used underscored binding _x"})
   (lint! "(if-some [_x 1] 1 2)" used-underscored-binding-linter))
  (assert-submaps '({:file "<stdin>", :row 1, :col 12, :level :warning, :message "used underscored binding _x"})
   (lint! "(when-let [_x 1] 1)" used-underscored-binding-linter))
  (assert-submaps '({:file "<stdin>", :row 1, :col 13, :level :warning, :message "used underscored binding _x"})
   (lint! "(when-some [_x 1] 1)" used-underscored-binding-linter))
  (assert-submaps '({:file "<stdin>", :level :warning, :message "used underscored binding _x"})
   (lint! "(for [_x []] x)" used-underscored-binding-linter))
  (assert-submaps '({:file "<stdin>", :level :warning, :message "used underscored binding _x"})
   (lint! "(doseq [_x []] x)" used-underscored-binding-linter))
  (assert-submaps '({:level :warning, :message "used underscored binding _x"}
                    {:level :warning, :message "used underscored binding _y"})
   (lint! "(with-open [_x ? _y ?] x y)" used-underscored-binding-linter))
  (assert-submaps '({:level :warning, :message "used underscored binding _x"})
   (lint! "(with-local-vars [_x 1] x)" used-underscored-binding-linter))
  (assert-submaps '({:file "<stdin>", :row 1, :col 24, :level :warning, :message "used underscored binding _x"})
   (lint! "(defmacro foo [] (let [_x 1] `(inc x)))" used-underscored-binding-linter))
  (assert-submaps '({:file "<stdin>", :row 1, :col 12, :level :warning, :message "used underscored binding _x"})
   (lint! "(defn foo [_x] (quote x))" used-underscored-binding-linter))
  (assert-submaps '({:file "<stdin>", :row 1, :col 17, :level :warning, :message "used underscored binding _variadic"})
   (lint! "(let [{^boolean _variadic :variadic?} {}] variadic)" used-underscored-binding-linter))
  (assert-submaps '({:file "<stdin>", :row 1, :col 8, :level :warning, :message "used underscored binding _a"})
   (lint! "#(let [_a %] a)" used-underscored-binding-linter))
  (assert-submaps '({:file "<stdin>", :row 1, :col 7, :level :warning, :message "used underscored binding _a"})
   (lint! "(let [_a 1] `{:a a})" used-underscored-binding-linter))
  (is (empty? (lint! "(let [x 1] _y)" used-underscored-binding-linter)))
  (is (empty? (lint! "(ns problem {:clj-kondo/config {:linters {:used-underscored-binding {:level :off}}}}) (defn f [_x] x)" used-underscored-binding-linter))))
