(ns coql.clause-spec
  (:require [clojure.spec.alpha :as s]
            [coql.group-rule-spec :as ds]))

(s/def ::type #{"clause"})

;;; SELECT
(s/def ::operator #{"select"})

(s/def ::value (s/or :string string? :vector vector?))

(s/def ::select-clause
  (s/keys :req-un [::type ::operator ::value]))

;;; WHERE
(s/def ::operator #{"where"})

(s/def ::value ds/any-rule)

(s/def ::where-clause
  (s/keys :req-un [::type ::operator ::value]))

;;; FROM
(s/def ::operator #{"from"})

(s/def ::value string?)

(s/def ::from-clause
  (s/keys :req-un [::type ::operator ::value]))

;;; LIMIT
(s/def ::operator #{"limit"})

(s/def ::value integer?)

(s/def ::limit-clause
  (s/keys :req-un [::type ::operator ::value]))

;;; OFFSET
(s/def ::operator #{"offset"})

(s/def ::value integer?)

(s/def ::offset-clause
  (s/keys :req-un [::type ::operator ::value]))

;;; ORDER BY
(s/def ::operator #{"order_by"})

(s/def ::value (s/or :string string? :vector vector?))

(s/def ::control #{"ASC" "DESC"})

(s/def ::order-by-clause
  (s/keys :req-un [::type ::operator ::value]
          :opt-un [::control]))

(defn- operator-keys
  [clauses]
  (map #(:operator %) clauses))

(defn- uniq-operator-keys?
  [clauses]
  (let [keys (operator-keys clauses)]
    (= (count keys) (count (set keys)))))

(defn- clause-is-valid?
  [clauses]
  (->> clauses
       (map (fn [clause] (condp = (:operator clause)
                           "select" (s/valid? ::select-clause clause)
                           "from" (s/valid? ::from-clause clause)
                           "where" (s/valid? ::where-clause clause)
                           "limit" (s/valid? ::limit-clause clause)
                           "offset" (s/valid? ::offset-clause clause)
                           "order_by" (s/valid? ::order-by-clause clause)
                           nil)))
       (every? some?)))

(s/def ::clauses
  (s/and uniq-operator-keys? clause-is-valid?))

(def clauses ::clauses)
