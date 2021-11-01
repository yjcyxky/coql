(ns coql.core-test
  (:require [clojure.test :refer [testing is deftest are]]
            [coql.core :as coql]))

;; Examples
(def rule-example-1
  {:type "rule"
   :query {:variable "test"
           :operator "in"
           :value ["a"]}})

(def rules-example-1
  {:type "group"
   :operator "and"
   :children [rule-example-1 rule-example-1]})

(def rule-invalid-example-1
  {:type "rule"
   :query {:variable "test"
           :operator "=="
           :value ["a"]}})

(def rules-invalid-example-1
  {:type "group"
   :operator ">="
   :children [rule-example-1 rule-example-1]})

(deftest test-parse-rules
  (testing "Parse rules."
    (are [x y] (= x y)
      "test IN [\"a\"]" (coql/parse-rules rule-example-1)
      "test IN [\"a\"] AND test IN [\"a\"]" (coql/parse-rules rules-example-1))))

(deftest test-check-rules
  (testing "Check rules."
    (is (some? (coql/check-rules rules-invalid-example-1)))
    (is (some? (coql/check-rules rule-invalid-example-1)))
    (is (nil? (coql/check-rules rule-example-1)))
    (is (nil? (coql/check-rules rules-example-1)))))

(def select-clause-example-1
  [{:type "clause"
    :operator "select"
    :value ["test1" "test2"]}])

(def where-clause-example-1
  [{:type "clause"
    :operator "where"
    :value rules-example-1}])

(def from-clause-example-1
  [{:type "clause"
    :operator "from"
    :value "test"}])

(def limit-clause-example-1
  [{:type "clause"
    :operator "limit"
    :value 20}])

(def offset-clause-example-1
  [{:type "clause"
    :operator "offset"
    :value 20}])

(def orderby-clause-example-1
  [{:type "clause"
    :operator "order_by"
    :value ["test1" "test2"]
    :control "DESC"}])

(deftest test-parse-clauses
  (testing "Parse clauses."
    (are [x y] (= x y)
      "SELECT test1, test2" (coql/parse-clauses select-clause-example-1)
      "FROM test" (coql/parse-clauses from-clause-example-1)
      "WHERE test IN [\"a\"] AND test IN [\"a\"]" (coql/parse-clauses where-clause-example-1)
      "LIMIT 20" (coql/parse-clauses limit-clause-example-1)
      "OFFSET 20" (coql/parse-clauses offset-clause-example-1)
      "ORDER BY test1, test2 DESC" (coql/parse-clauses orderby-clause-example-1))))

(deftest test-check-clausess
  (testing "Check clauses."
    (is (nil? (coql/check-clauses select-clause-example-1)))
    (is (nil? (coql/check-clauses from-clause-example-1)))
    (is (nil? (coql/check-clauses where-clause-example-1)))
    (is (nil? (coql/check-clauses limit-clause-example-1)))
    (is (nil? (coql/check-clauses offset-clause-example-1)))
    (is (nil? (coql/check-clauses orderby-clause-example-1)))))