(ns coql.rule-spec
  (:require [clojure.spec.alpha :as s]))

;; ------------------------------------------ Rule ------------------------------------------
(s/def ::type #{"rule"})

(s/def ::operator #{"=" ">" "<" ">=" "<=" "in" "not in" "!=" "regex"})

(s/def ::variable string?)

(s/def ::value any?)

(s/def ::query
  (s/keys :req-un [::variable ::operator ::value]))

(s/def ::rule
  (s/keys :req-un [::type ::query]))

(def rule ::rule)
