(ns coql.group-rule-spec
  (:require [clojure.spec.alpha :as s]
            [coql.rule-spec :as rs]))

;; ------------------------------------------ Group Rule ------------------------------------------
(s/def ::type #{"group"})

(s/def ::operator #{"and" "or"})

(s/def ::vector-entry
  (s/or :rule rs/rule :group-rule ::group-rule))

(s/def ::children
  (s/coll-of ::vector-entry :kind vector))

(s/def ::group-rule
  (s/keys :req-un [::type ::operator ::children]))


;; ------------------------------------------ Reference Rule --------------------------------------
(s/def ::any-rule
  (s/or :rule rs/rule :group-rule ::group-rule :empty empty?))

(def group-rule ::group-rule)

(def any-rule ::any-rule)

(def children ::children)
