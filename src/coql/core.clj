(ns coql.core
  (:require [clojure.spec.alpha :as s]
            [coql.group-rule-spec :as ps]
            [clojure.string :as clj-str]))

;; Examples
(def example
  {:type "group"
   :operator "and"
   :children [{:type "rule"
               :query {:variable "tissue_type"
                       :operator "in"
                       :value ["TumorTissue"]}}
              {:type "rule"
               :query {:variable "project_name"
                       :operator "not in"
                       :value ["TNBC" "CBCGA"]}}
              {:type "rule"
               :query {:variable "experimental_strategy"
                       :operator "in"
                       :value ["RNA_seq"]}}]})

(s/check-asserts true)

(def rule-example
  {:type "rule"
   :query {:variable "test"
           :operator "in"
           :value ["a"]}})

(def rules-example
  {:type "group"
   :operator "and"
   :children [rule-example rule-example]})


;; Predictor
(defn count-gte-2?
  [coll]
  (>= (count coll) 2))

(defn string|number?
  [value]
  (or (string? value) (number? value)))

(defn rules?
  [rules]
  (s/valid? ps/group-rule rules))

;; Parser
(defn format-rule
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
  [operator]
  (clj-str/replace operator #" " "_"))

(defn dispatch
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

(defn check-rules
  [rules]
  (s/explain-data ps/any-rule rules))

(defn parse-rule
  [rule]
  (let [{:keys [type query]} rule]
    (if (= type "rule")
      (dispatch query)
      rule)))

(defn parse
  [rules]
  {:pre [(s/assert ps/any-rule rules)]}
  (let [{:keys [type operator children]} rules]
    (cond
      (= type "group") (cond
                         (= operator "and") (format-and (map parse children))
                         (= operator "or") (format-or (map parse children)))
      (= type "rule") (parse-rule rules)
      :else "")))
