(ns coql.core-test
  (:require [clojure.test :refer :all]
            [coql.core :as core]))

(deftest test-parse
  (testing "Parse group rule."
    (is (= "tissue_type IN [\"TumorTissue\"] AND project_name NOT IN [\"TNBC\" \"CBCGA\"] AND experimental_strategy IN [\"RNA_seq\"]" 
           (core/parse core/example)))))
