(ns
  ^{:author "Tom Sanders",
    :doc    "Define domain types"}
  entity.core
  (:require [clojure.string :as s]
            #?(:clj [typeops.assign :as t])
            [entity.protocol :refer [IO read-key write-val delete-val]]))

(deftype ^:no-doc NullIO []
  IO
  (read-key [_ key-val]
    nil)
  (read-key [_ key-name key-val]
    nil)
  (write-val [_ entity-val]
    nil)
  (delete-val [_ entity-val]
    nil))

(def null-io
  "I/O that does nothing"
  (->NullIO))

; List of all types.
(defonce ^:no-doc types
         (atom {}))

(defn- enum?
  [enum]
  (-> (meta enum)
      :enum?))

(defn- default-enum-val
  "Return the default value for the given domain enum map"
  [enum]
  (if-not (enum? enum)
    (throw (ex-info (str "Not an enum") {:enum (or enum "nil")})))
  (-> (meta enum)
      :default
      enum))

;TODO: We can't refer to Type.Field at the moment. Is this especially useful?
(defn ^:no-doc resolve-ref
  "If the argument is a keyword, assume it is a reference to a type, look
  it up and return the mapping. Otherwise just return the argument"
  [arg]
  (if (keyword? arg)
    (let [v (arg @types)]
      (if (nil? v)
        (throw (ex-info "Unresolved reference" {:arg arg})))
      (if (enum? v)
        (default-enum-val v)
        v))
    arg))

(defn- make-enum
  [symvals default]
  (let [meta (reduce-kv
               (fn [m k v]
                 (assoc m v k))
               {:enum?   true
                :default default}
               symvals)]
    (with-meta symvals meta)))

(defn- namespaced?
  "Throws if arg is not a namespaced keyword"
  [arg]
  (if-not (and (keyword? arg) (namespace arg))
    (throw (ex-info "Not a name-spaced keyword" {:name arg}))))
; todo called from macros means ex-info is not evaluated. Any others?

; Not necessary this is a macro, but leaves open future expansion if we
; make it so, for example rendering width hints or what-have-you.
(defmacro defscalar
  "Define a scalar type, for example:

    (defscalar :foo/Money 0.00M

  defines the type :foo/Money as a big decimal with 2 decimal
  places of accuracy."
  [name val]
  (namespaced? name)
  (let [v (resolve-ref val)]
    `(do
       (swap! types assoc ~name ~v)
       ~name)))

; Make an enum type. Symbolic access to enum vals using a keyword.
; Meta data contains reverse mapping and default symbol
(defmacro defenum
  "Define an enum type, mapping keywords to values, for example

    (defenum :foo/Active {:y 1
                          :n 0} :y)

  defines the enum :foo/Active with two keyword symbols and
  corresponding values, and whose default is :y"
  [name symsvals default]
  (namespaced? name)
  `(let [default# ~default
         syms# ~symsvals
         meta# (reduce-kv
                 (fn [~'m ~'k ~'v]
                   (assoc ~'m ~'v ~'k))
                 {:enum?   true
                  :default default#}
                 syms#)]
     (assert (contains? syms# default#) "Default value not valid")
     (swap! types assoc ~name (with-meta syms# meta#))
     ~name))

(defn- enum-val-impl
  "Return the value for the given domain enum type and symbol"
  [enum sym]
  (let [enum (enum @types)]
    (if-not (enum? enum)
      (throw (ex-info (str "Not an enum") {:enum (or enum "nil")})))
    (or (sym enum)
        (throw (ex-info (str "Unknown enum symbol") {:sym sym})))))

(defn enum-val-fn
  "Return the value for the given domain enum type and symbol"
  [enum sym]
  (enum-val-impl enum sym))

(defmacro enum-val
  "Return the value for the given domain enum type and symbol"
  [enum sym]
  (enum-val-impl enum sym))

(defmacro enum-sym
  "Return the symbol for a given domain enum type and value"
  [enum val]
  (let [enum (enum @types)]
    (if-not (enum? enum)
      (throw (ex-info (str "Not an enum") {:enum (or enum "nil")})))
    (or (-> (meta enum)
            (get val))
        (throw (ex-info (str "Unknown enum value") {:val val})))))

(defn- field-info
  "Processes a field name, which looks at most like :ns/Type.Field,
  returning a map containing the :name and
  any dependent :type/:field this field declares"
  [f-name]
  (let [name-space (namespace f-name)
        unqual (name f-name)
        [ref-type ref-field] (s/split unqual #"\.")]
    (if (and (nil? name-space) ref-type ref-field)
      (throw (ex-info "Field reference must be namespace qualified" {:arg f-name})))
    (if (and name-space (nil? ref-field))
      (throw (ex-info "Field reference must be in the form ns/Type.Field" {:arg f-name})))
    (assoc {} :name (keyword (or ref-field unqual))
              :type (and ref-field (keyword name-space ref-type))
              :field (keyword ref-field))))

(defn- add-deps
  "Returns the argument with any dependent type/field information
  conj'd"
  [deps field-info f-name]
  (let [{:keys [type field name]} field-info]
    (if (and type field)
      (assoc deps f-name {:type type :field field})
      (assoc deps f-name {:type :this :field name}))))

(defn- key-proto
  "Process the :fields element of a key definition into its prototype map. Gather
  any dependencies the declarations infer"
  [fields]
  (loop [fields fields
         proto {}
         deps {}]
    (if (seq fields)
      (let [[v w x y z] fields
            field-info (field-info v)]
        (cond
          (and (= :as w) (= '= y))                          ; field :as alias = value
          (recur (drop 5 fields)
                 (assoc proto (keyword x) z)
                 (add-deps deps field-info (keyword x)))
          (= '= w)                                          ; field = value
          (recur (drop 3 fields)
                 (assoc proto (:name field-info) x)
                 (add-deps deps field-info (:name field-info)))
          (= :as w)                                         ; field :as alias
          (recur (drop 3 fields)
                 (assoc proto (keyword x) nil)
                 (add-deps deps field-info (keyword x)))
          :else
          (recur (drop 1 fields)                            ; field
                 (assoc proto (:name field-info) nil)
                 (add-deps deps field-info (:name field-info)))))
      {:proto proto :deps deps})))

(defn- fields-proto
  "Process the fields of the entity definition into its prototype map."
  [fields]
  (loop [fields fields
         proto {}
         type-proto {}
         field-names []]
    (if (seq fields)
      (let [[w x y z] fields]
        (cond
          (= '= y)                                          ; field type = value
          (recur (drop 4 fields)
                 (assoc proto (keyword w) `(resolve-ref ~z))
                 (assoc type-proto (keyword w) `(resolve-ref ~x))
                 (conj field-names w))
          :else
          (let [val `(resolve-ref ~x)]
            (recur (drop 2 fields)                          ; field type|expr
                   (assoc proto (keyword w) val)
                   (assoc type-proto (keyword w) val)
                   (conj field-names w)))))
      {:proto       proto
       :field-names field-names
       :type-proto  type-proto})))

(defn- ensure-fn
  [f]
  `(if (fn? ~f)
     ~f
     (throw (ex-info "not a function: " {:arg ~f}))))

(defn- ensure-keyword
  [k]
  (if (keyword? k)
    k
    (throw (ex-info "not a keyword: " {:arg k}))))

(defn- primary-key-proto
  "Make a prototype primary key from the entity proto
  and primary fields"
  [proto primary val?]
  (reduce #(assoc %1 (keyword %2) (if val? ((keyword %2) proto)
                                           nil))
          {}
          primary))

(defn- parse-extras
  [proto primary extras]
  (let [primary-key-info {:primary {:unique? true,
                                    :cached? true,
                                    :fields  {:proto      (primary-key-proto proto primary false)
                                              :type-proto (primary-key-proto proto primary true)
                                              :deps       {}}
                                    :name    :primary}}
        result
        (loop [extras extras
               result {}]
          (if (seq extras)
            (let [[k v] extras]
              (cond
                (nil? v)
                (throw (ex-info "incorrect syntax: " {:key k}))
                (= :io k)
                (recur (drop 2 extras)
                       (assoc result :io v))
                (= :keys k)
                (recur (drop 2 extras)
                       (assoc result :keys (reduce-kv #(assoc
                                                         %1
                                                         %2
                                                         (assoc %3 :fields (key-proto (:fields %3))
                                                                   :name %2))
                                                      primary-key-info
                                                      v)))
                (or (= :create k)
                    (= :mutate k)
                    (= :join k)
                    (= :destroy k))
                (recur (drop 2 extras)
                       (assoc result k (ensure-fn v)))
                (= :alias k)
                (recur (drop 2 extras)
                       (assoc result k (ensure-keyword v)))
                :else
                (throw (ex-info "Unknown keyword " {:arg k}))))
            result))]
    (if-not (:keys result)                                  ; may have no other keys
      (assoc result :keys primary-key-info)
      result)))

(defmacro defentity
  "Define a record type with fields, keys and any additional information"
  [ent-name fields primary & more]
  (do
    (if-not (and (keyword? ent-name) (namespace ent-name))
      (throw (ex-info "name must be name-spaced keyword" {:name ent-name})))
    ;(if-not (and (vector? fields) (seq fields) (even? (count fields)))
    ;  (throw (ex-info "fields must be a vector of name/value pairs" {:fields fields})))
    (if-not (and (vector? primary) (seq primary))
      (throw (ex-info "primary key not a vector or empty" {:primary primary})))
    (let [proto-info# (fields-proto fields)
          fields# (:field-names proto-info#)
          rec-name# (name ent-name)]
      (if-not (every? (set fields#) primary)
        (throw (ex-info "primary fields not in declared fields" {:primary primary
                                                                 :fields  fields#})))
      (let [proto# (:proto proto-info#)
            type-proto# (:type-proto proto-info#)
            extras# (parse-extras proto# primary more)]
        `(do
           (defrecord ~(symbol rec-name#) ~fields#)
           (swap! types assoc
                  ~ent-name
                  (with-meta
                    {:name       ~ent-name
                     :ctor       (eval (symbol (str "->" ~rec-name#))) ;ctor eg ->Currency
                     :map-ctor   (eval (symbol (str "map->" ~rec-name#))) ;from a map eg map->Currency
                     :proto      ((resolve (symbol (str "map->" ~rec-name#))) (t/merge ~type-proto# ~proto#)) ;exemplar
                     :type-proto ~type-proto#
                     :primary    (vec (map keyword '~primary))
                     :extras     ~extras#}
                    {:type? true}))



           ~(symbol rec-name#))))))

(defn find-entity
  "Find the type from the given keyword ref"
  [entity]
  (if (nil? entity)
    (throw (ex-info "entity cannot be nil" {})))
  (let [l-entity
        (cond
          (keyword? entity) (entity @types)
          :else entity)]
    (if-not (-> l-entity
                meta
                :type?)
      (throw (ex-info "Not a type or unknown" {:entity entity}))
      l-entity)))

(defn find-value
  "Find a value from the types catalog. Two args means type:field.
  Single arg means scalar. If type:field does not resolve then field is
  considered a key name, returning the key proto if found."
  ([scalar] (find-value nil scalar))
  ([entity field]
   (let [type-info (if entity
                     (-> (entity @types)
                         :proto
                         field)
                     (field @types))]
     type-info)))

(defn- find-field-value
  "Look for the specified key field's typed value. This will
  either be:
    1) a value from the entity prototype
    2) a value from the referenced entity prototype
  A value of nil is allowed ... (does this make sense?)"    ; TODO <--
  [key-field type-proto dep]
  (let [{:keys [type field]} dep
        proto (if (= :this type)
                type-proto
                (:proto (find-entity type)))]
    (if (contains? proto field)
      (field proto)
      :nil)))


(defn- resolve-key-fields
  "Using any dependencies try to make the given key's typed
  prototype. If successful preserve it in the key meta data
  for future use. Throws on first failure to resolve."
  [entity key-info]
  (let [type-proto (:proto entity)
        key-fields (-> key-info
                       :fields
                       :proto
                       keys)
        deps (-> key-info
                 :fields
                 :deps)
        key-proto (reduce
                    #(let [v (find-field-value
                               %2
                               type-proto
                               (%2 deps))]
                       (if (= v :nil)
                         (reduced %2)
                         (assoc %1 %2 v)))
                    {}
                    key-fields)]
    (if (map? key-proto)
      key-proto
      (throw (ex-info "Cannot resolve key field"
                      {:entity (:name entity)
                       :key    (:name key-info)
                       :field  key-proto})))))


(defn get-key-info
  [entity key-name]
  (let [l-entity (find-entity entity)
        key-info (-> l-entity
                     :extras
                     :keys
                     key-name)]
    (if-not key-info
      (throw (ex-info "Key not found" {:entity entity :key key-name})))
    key-info))

(defn- key-typed-proto
  "Look for the typed proto for the given key. If available,
  return it. Otherwise try to create it using any field dependencies
  and keep for future use."
  [entity key-name]
  (let [key-info (get-key-info entity key-name)]
    (or (-> key-info
            :fields
            :type-proto)
        (let [type-proto (resolve-key-fields entity key-info)]
          (swap! types
                 assoc-in [(:name entity)
                           :extras
                           :keys
                           key-name
                           :fields
                           :type-proto] type-proto)
          type-proto))))

(defn- apply-meta
  "Carry type information in the meta data of a key value
  or instance."
  ([instance entity] (apply-meta instance entity nil))
  ([instance entity key-name]
   (-> instance
       (with-meta (into {}
                        (for [[c k v] (partition 3 [true :entity (:name entity)
                                                    key-name :key key-name
                                                    (not key-name) :proto (:type-proto entity)
                                                    key-name :proto (and key-name (key-typed-proto entity key-name))
                                                    key-name :unique? (and key-name ((get-key-info entity key-name)
                                                                                      :unique?))])
                              :when c]
                          [k v]))))))

; TODO: consider find-entity viability
(defn- get-entity-def
  "Return the defnition of the given entity. The argument can be
   - keyword eg :foo/Fruit returns the definition of :foo/Fruit
   - already the defintion itself, returns the argument
   - an instance of the entity (from which the defintion can be found)"
  [instance]
  (cond
    (keyword? instance) (find-entity instance)
    (and (:type? (meta instance)) (:proto instance)) instance
    :else (find-entity (:entity (meta instance)))))

(defn key-val?
  "Returns true if the argument appears to be a value returned
  by make-key"
  [arg]
  (let [{:keys [entity key-name proto]} (meta arg)]
    (and entity key-name proto true)))

(defn entity-instance?
  "Returns true if the argument is an instance of the given entity. The
  entity can be a keyword (a literal referral to the domain type) or
  an instance (from which the type will be determined).
  If arg is not an instance of entity, returns false."
  [arg entity]
  (and (keyword? arg) (throw (ex-info "arg cannot be a keyword" {})))
  (= (-> (meta arg)
         :entity)
     (-> (get-entity-def entity)
         :name)))


(defn make-key
  "Make a key value. Returns a map comprising only the map keys
  for the given domain key name. Any missing fields are
  nil (or explicit default value); additional
  fields are ignored."
  [key-val entity key-name]
  (if (nil? key-val)
    (throw (ex-info "key-val cannot be nil" {})))
  (let [entity (find-entity entity)
        key-proto (-> (get-key-info entity key-name)
                      :fields
                      :proto)
        key-fields (or (keys key-proto) '())
        key-val (select-keys key-val key-fields)]
    (-> key-proto
        (apply-meta entity key-name)
        (#?(:clj t/merge :cljs merge) key-val))))

(defn primary-key-to-meta
  "Augment the instance meta data with the primary key.
  This can only be done when the primary key is established,
  but it is trusted this is so"
  [instance]
  (let [cur-meta (meta instance)]
    (vary-meta
      instance
      #(assoc %1
         :primary
         (make-key
           instance
           (:entity cur-meta)
           :primary)))))

(defn get-alias
  "Return a keyword that can be used to associate the
  instance in a map. This will be either its unqualified
  name or any alias that was specified when defined."
  [instance]
  (let [entity (get-entity-def instance)]
    (or (-> entity
            :extras
            :alias)
        (-> entity
            :name
            name
            keyword))))

(defn get-create-fn
  "Return the function that can be called to correctly initialise this
  and anything other required in the domain."
  [instance]
  (-> (get-entity-def instance)
      :extras
      :create))

(defn get-mutate-fn
  "Return the function that can be called to verify what changes are
   allowed and perform any consequential actions in the domain."
  [instance]
  (-> (get-entity-def instance)
      :extras
      :mutate))

(defn get-destroy-fn
  "Return the function that can be called to destroy this
  and anything else required in the domain."
  [instance]
  (-> (get-entity-def instance)
      :extras
      :destroy))

(defn new-instance
  "Make an instance of the specified type, setting any fields
  when an initial value is given. Any missing fields remain their
  default values; additional fields are ignored."
  ([entity] (new-instance entity {}))
  ([entity ent-val]
   (let [entity (find-entity entity)
         proto (:proto entity)
         ent-val (select-keys ent-val (keys proto))]
     (-> proto
         (apply-meta entity)
         (#?(:clj t/merge
             :cljs merge) ent-val)))))

(defn get-primary-key
  "Return the instance's primary key from its meta data.
  If not found, throws, if identifiable as the primary key
  itself then returns the argument"
  [instance]
  (or (-> instance
          meta
          :primary)
      (and (-> instance
               meta
               :key
               (= :primary))
           instance)
      (throw (ex-info (str "Primary key not set") {:instance instance
                                                   :meta     (meta instance)}))))
(defn read-entity
  "Read the given entity, applying the key value"
  ([key-val]
   (let [key-meta (meta key-val)]
     (read-entity key-val (:entity key-meta) (:key key-meta))))
  ([entity-name key-val]
   (read-entity key-val
                entity-name
                (-> key-val
                    meta
                    :key)))
  ([key-val entity-name key-name]
   (if-not (and entity-name key-name key-val)
     (throw (ex-info "entity-name, key-name and value cannot be null" {})))
   (if-let [entity (find-entity entity-name)]
     (let [result (-> entity
                      :extras
                      :io
                      (read-key key-name key-val))
           add-meta (fn [instance]
                      (-> (new-instance entity instance)
                          (apply-meta entity)
                          (primary-key-to-meta)))]
       (cond
         (nil? result) result
         (map? result) (add-meta result)
         :else (map add-meta result)))
     (throw (ex-info "No such entity of name:" entity-name)))))

(defn write-instance
  "Write the given entity instance to its persistent storage"
  [instance]
  (if-let [entity (:entity (meta instance))]
    (let [entity (find-entity entity)]
      (-> entity
          :extras
          :io
          (write-val instance)))
    (throw (ex-info "Not an entity instance:" {:arg instance}))))

(defn delete-instance
  "Delete the given entity instance from its persistent storage."
  [instance]
  (if-let [entity (:entity (meta instance))]
    (let [entity (find-entity entity)]
      (-> entity
          :extras
          :io
          (delete-val instance)))
    (throw (ex-info "Not an entity instance:" {:arg instance}))))
