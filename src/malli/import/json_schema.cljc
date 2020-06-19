(ns malli.import.json-schema
  (:require [malli.core :as m]
            [clojure.spec.alpha :as s]))

(declare import-conformed-schema)

(m/validate [any?] nil)
(m/validate [:and [:string {:min 4}] [:re "\\d+"]] "12")

(s/def ::minLength integer?)
(s/def ::maxLength integer?)
(s/def ::pattern string?)

(defmulti basic-types :type)

(defmethod basic-types "string" [_]
  (s/keys :opt-un [::minLength ::maxLength ::pattern]))

(defmethod basic-types "boolean" [_]
  (constantly true))

(defmethod basic-types "null" [_]
  (constantly true))

(s/def ::minimum number?)
(s/def ::maximum number?)
(s/def ::exclusiveMinimum number?)
(s/def ::exclusiveMaximum number?)

(s/def ::numeric-opts 
  (s/keys :opt-un [::minimum ::maximum ::exclusiveMinimum ::exclusiveMaximum]))

(defmethod basic-types "integer" [_]
  ::numeric-opts)
(defmethod basic-types "number" [_]
  ::numeric-opts)

(s/def ::required (s/coll-of string? :distinct true :into #{}))
(s/def ::additionalProperties boolean?)
(s/def ::properties (s/map-of keyword? ::json-schema))

(defmethod basic-types "object" [_]
  (s/keys :opt-un [::properties ::additionalProperties ::required]))

(s/def ::items (s/or :homo ::json-schema :hetero (s/coll-of ::json-schema)))
(s/def ::minItems integer?)
(s/def ::maxItems integer?)

(defmethod basic-types "array" [_]
  (s/keys :opt-un [::items ::minItems ::maxItems]))

(s/def ::json-schema 
  (s/or :always-pass true?
        :always-fail false?
        :normal (s/multi-spec basic-types :type)))

(comment
  (fn retag [gen-v dispatch-tag]
    [(keyword dispatch-tag) gen-v]))

(s/conform ::json-schema {:type "object" 
                          :properties {:foo true 
                                       :bar {:type "boolean"}}
                          :required ["foo"]})

(defn import-string-schema [{:keys [minLength maxLength pattern]}]
  (let [len-constraint (cond-> {}
                         minLength (assoc :min minLength)
                         maxLength (assoc :max maxLength))
        main-clause [:string len-constraint]
        regex-clause [:re pattern]
        no-regex (nil? pattern)]
    (if no-regex
      main-clause
      [:and main-clause regex-clause])))

(import-string-schema {:minLength 3 :pattern "\\d+"})

(defn import-object-schema
  [{:keys [properties additionalProperties required]
    :or {properties {} additionalProperties true required #{}}}]
  (let [body [:map (if (not additionalProperties) {:closed true} {})]]
    (into body (for [[k v] properties]
                 (if (contains? required (name k))
                   [k (import-conformed-schema v)]
                   [k {:optional true} (import-conformed-schema v)])))))

(let [m {:foo 3 :bar "hello"}]
  (into [:map {:test 1}] (for [[k v] m] v)))

(m/validate [:sequential {:min 1 :max 5} [any?]] [1 2 nil :foo])
(m/validate [:tuple [any?] integer? string?] [:hi 23 "bye" 12])

(defn import-array-schema [{:keys [items minItems maxItems]}]
  (if (or (nil? items) (= :homo (first items)))
    (let [constraints (cond-> {}
                        minItems (assoc :min minItems)
                        maxItems (assoc :max maxItems))]
      [:sequential constraints (if (nil? items)
                                 [any?]
                                 (import-conformed-schema (second items)))])
    (into [:tuple] (map import-conformed-schema (second items)))))

(defn import-numeric-schema [t {:keys [minimum maximum exclusiveMinimum exclusiveMaximum]}]
  (let [main-pred (case t :number number? :integer integer?)
        constraints (cond-> []
                      minimum (conj [:>= minimum])
                      maximum (conj [:<= maximum])
                      exclusiveMinimum (conj [:> exclusiveMinimum])
                      exclusiveMaximum (conj [:< exclusiveMaximum]))]
    (if (empty? constraints)
      [main-pred]
      (into [:and [main-pred]] constraints))))

(defn import-conformed-schema [j]
  (let [[b data] j
        datak (keyword (:type data))]
    (case [b datak]
      [:always-pass nil] [any?]
      [:always-fail nil] [any?]
      [:normal :null] [nil?]
      [:normal :string] (import-string-schema data)
      [:normal :boolean] [boolean?]
      [:normal :number] (import-numeric-schema :number data)
      [:normal :integer] (import-numeric-schema :integer data)
      [:normal :object] (import-object-schema data)
      [:normal :array] (import-array-schema data))))

(defn import-schema [j]
  (import-conformed-schema (s/conform ::json-schema j)))

(m/validate (import-schema {:type "boolean"}) 4)

(def obj-s1
  (import-schema {:type "object"
                  :properties {:foo true
                               :bar {:type "boolean"}}
                  :required ["bar"]}))

obj-s1

(m/validate obj-s1
            {:foo {:s "tt" :x [1 2]} :bar false :extra "hey"})

(m/validate (import-schema {:type "number" :minimum 3 :exclusiveMaximum 7})
            7.0)

(import-schema {:type "integer"})

(m/validate [nil?] nil)

(m/validate (import-schema {:type "array" 
                            :items [{:type "boolean"} {:type "number" :minimum 3.4} true]})
            [true 3.5 :ghhh])

(import-schema {:type "array" :items {:type "string"} :minItems 5})

(import-schema {:type "array"})
