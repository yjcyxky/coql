(ns coql.core
  "Turn query json into SQL. It contains two type of element, group rule and rule, in the query json.

  (def rules [{:type \"clause\"
               :operator \"select\"
               :value [\"test1\" \"test2\"]}
              {:type \"clause\"
               :operator \"from\"
               :value \"test\"}
              {:type \"clause\"
               :operator \"where\"
               :value {:type \"group\",
                       :operator \"and\",
                       :children [{:type \"rule\",
                                   :query {:variable \"test1\",
                                   :operator \"in\",
                                   :value [\"a\"]}},
                                  {:type \"rule\",
                                   :query {:variable \"test2\",
                                   :operator \"not in\",
                                   :value [\"a\"]}}]}
              {:type \"clause\"
               :operator \"order_by\"
               :value [\"test1\"]
               :control \"ASC\"}
              {:type \"clause\"
               :operator \"limit\"
               :value 10}
              {:type \"clause\"
               :operator \"offset\"
               :value 1}]

   (require '[coql.core :as coql])
   (= \"SELECT test1, test2 FROM test WHERE test1 in ['a'] AND test2 not in ['a'] ORDER BY test1 ASC LIMIT 10 OFFSET 1\"
      (coql/parse rules))

   ChangeLog:
     v0.1.0: Support WHERE clause.
     v0.1.1: Support SELECT/LIMIT/OFFSET clause.
  "
  (:require [clojure.spec.alpha :as s]
            [coql.group-rule-spec :as ps]
            [coql.rule-spec :as rs]
            [coql.clause-spec :as cs]
            [clojure.string :as clj-str])
  (:import [clojure.lang PersistentVector]))

(s/check-asserts true)

;; Predictor
(defn count-gte-2?
  "A predictor for checking the number of items in collection whether is greater than 2."
  [coll]
  (>= (count coll) 2))

(defn string|number?
  "A predictor for checking the value whether is a string or number?"
  [value]
  (or (string? value) (number? value)))

(defn group-rule?
  "A predictor for checking the group rule whether is valid."
  [rule]
  (s/valid? ps/group-rule rule))

(defn rule?
  "A predictor for checking the group rule whether is valid."
  [rule]
  (s/valid? rs/rule rule))

(defn any-rule?
  "A predictor for checking the group rule whether is valid."
  [rule]
  (s/valid? ps/any-rule rule))


;; Rule Parser
(defn format-rule
  "Convert the rule in hash-map to a eligible SQL string."
  [operator variable value]
  (format "%s %s %s" variable operator value))

(defn format-eq
  [variable value]
  {:pre [(string|number? value)]}
  (format-rule "=" variable value))

(defn format-gt
  [variable value]
  {:pre [(string|number? value)]}
  (format-rule ">" variable value))

(defn format-gte
  [variable value]
  {:pre [(string|number? value)]}
  (format-rule ">=" variable value))

(defn format-lt
  [variable value]
  {:pre [(string|number? value)]}
  (format-rule "<" variable value))

(defn format-lte
  [variable value]
  {:pre [(string|number? value)]}
  (format-rule "<=" variable value))

(defn format-ne
  [variable value]
  {:pre [(string|number? value)]}
  (format-rule "<>" variable value))

(defn format-in
  [variable value]
  {:pre [(vector? value)]}
  (format-rule "IN" variable value))

(defn format-nin
  "TODO: It's not safety."
  [variable value]
  {:pre [(vector? value)]}
  (format-rule "NOT IN" variable value))

(defn format-regex [variable value]
  {:pre [(string? value)]}
  (format-rule "LIKE" variable value))

(def rule-func-map
  {:= format-eq
   :> format-gt
   :>= format-gte
   :< format-lt
   :<= format-lte
   :!= format-ne
   :in format-in
   :not_in format-nin
   :regex format-regex})

(defn standardize
  "Convert the operator name to a standardized format, such as `not in` --> `not_in`."
  [operator]
  (clj-str/replace operator #" " "_"))

(defn dispatch
  "Find the appropriated formater based on the operator in rule to format the rule to a eligible SQL string."
  ([operator variable value]
   (let [func (rule-func-map (keyword (standardize operator)))]
     (func variable value)))
  ([rule]
   (let [{:keys [variable operator value]} rule]
     (dispatch operator variable value))))

(defn format-and
  ([rules]
   {:pre [(count-gte-2? rules)]}
   (reduce #(str %1 " AND " %2) rules))
  ([rule1 rule2 & rules]
   {:pre [(s/assert ps/any-rule rule1) (s/assert ps/any-rule rule2) (s/assert ps/children rules)]}
   (format-and (concat (str rule1 " AND " rule2) rules))))

(defn format-or
  ([rules]
   {:pre [(count-gte-2? rules)]}
   (apply #(str %1 " OR " %2) rules))
  ([rule1 rule2 & rules]
   {:pre [(s/assert ps/any-rule rule1) (s/assert ps/any-rule rule2) (s/assert ps/children rules)]}
   (format-or (concat (str rule1 " OR " rule2) rules))))

(defn parse-rule
  [rule]
  (let [{:keys [type query]} rule]
    (if (= type "rule")
      (dispatch query)
      rule)))

(defn parse-rules
  [rules]
  {:pre [(s/assert ps/any-rule rules)]}
  (let [{:keys [type operator children]} rules]
    (cond
      (= type "group") (cond
                         (= operator "and") (format-and (map parse-rules children))
                         (= operator "or") (format-or (map parse-rules children)))
      (= type "rule") (parse-rule rules)
      :else "")))

(defn check-rules
  "Check the rule whether is valid."
  [rules]
  (s/explain-data ps/any-rule rules))


;;; Clause Parser
(defn format-items
  [value]
  (condp instance? value
    PersistentVector (clj-str/join ", " value)
    String value))

(defn format-select
  [value]
  (format "SELECT %s" (format-items value)))

(defn format-from
  [value]
  (format "FROM %s" value))

(defn format-where
  [value]
  (when (any-rule? value)
    (format "WHERE %s" (parse-rules value))))

(defn format-limit
  [value]
  (format "LIMIT %s" value))

(defn format-offset
  [value]
  (format "OFFSET %s" value))

(defn format-orderby
  [value & control]
  (if control
    (format "ORDER BY %s %s" (format-items value) (first control))
    (format "ORDER BY %s" (format-items value))))

(defmulti sql-clause (fn [x] (keyword (:operator x))))

(defmethod sql-clause :select [clause]
  (format-select (:value clause)))

(defmethod sql-clause :where [clause]
  (format-where (:value clause)))

(defmethod sql-clause :from [clause]
  (format-from (:value clause)))

(defmethod sql-clause :limit [clause]
  (format-limit (:value clause)))

(defmethod sql-clause :offset [clause]
  (format-offset (:value clause)))

(defmethod sql-clause :order_by [clause]
  (format-orderby (:value clause) (:control clause)))

(defn check-clauses
  "Check the clause whether is valid."
  [clauses]
  (s/explain-data cs/clauses clauses))

(def ^:private clause-weight
  {:select 1
   :from 2
   :where 3
   :order_by 4
   :limit 5
   :offset 6})

(defn filter-sort-clauses
  [clauses & {:keys [keep-invalid?] :or {keep-invalid? true}}]
  (let [keyfn (fn [clause] (or ((keyword (:operator clause)) clause-weight)
                               100))
        filter-fn (fn [clause] (or keep-invalid?
                                   (and (some? (:operator clause))
                                        (= (:type clause) "clause"))))]
    (->> clauses
         (filter filter-fn)
         (sort-by keyfn <))))

(defn parse-clauses
  "Parse the json clauses to a valid sql clauses.
   
   Return the sql clauses if the json clauses is valid, 
   else the problems or nil when ignore? is true."
  [clauses & {:keys [ignore?] :or {ignore? false}}]
  (let [results (check-clauses clauses)
        output (if ignore? nil results)]
    (if (nil? results)
      (->> clauses
           (filter-sort-clauses)
           (map sql-clause)
           (clj-str/join " "))
      output)))
