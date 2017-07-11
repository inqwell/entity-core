(ns
  ^{:author "Tom Sanders",
    :doc    "Define domain types"}
  entity.core
  (:require [clojure.string :as s]
            [typeops.assign :as t]
            [com.rpl.specter :as sp]
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
(defn- resolve-ref
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

(defn enum-val
  "Return the value for the given domain enum type and symbol"
  [enum sym]
  (let [enum (enum @types)]
    (if-not (enum? enum)
      (throw (ex-info (str "Not an enum") {:enum (or enum "nil")})))
    (or (sym enum)
        (throw (ex-info (str "Unknown enum symbol") {:sym sym})))))

; name must be name-spaced
; fields is a vector of symbols and evaluated values
; primary is a vector of symbols all of which must be fields
; keys is a map of keywords to key definition maps
(deftype Entity
  [name ctor map-ctor proto primary extras])

;(defprotocol LifeCycle
;  (create [entity])
;  (join [entity])
;  (mutate [old-entity new-entity])
;  (destroy [entity]))

(defn- field-info
  "Processes a field name, which looks at most like :ns/Type.Field,
  returning a map containing the :name and
  any dependent :type/:field this field declares"
  [f-name]
  (let [name-space (namespace f-name)
        unqual (name f-name)
        [ref-type ref-field] (s/split unqual #"\.")]
    (if (and (nil? name-space) ref-type ref-field)
      (throw (IllegalArgumentException.
               (str "Field reference must be namespace qualified: " f-name))))
    (if (and name-space (nil? ref-field))
      (throw (IllegalArgumentException.
               (str "Field reference must be in the form ns/Type.Field: " f-name))))
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
                 (assoc proto (keyword w) (resolve-ref z))
                 (assoc type-proto (keyword w) (resolve-ref x))
                 (conj field-names w))
          :else
          (let [val (resolve-ref x)]
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

(defmacro defentity [ent-name fields primary & more]
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
                     :proto      ((resolve (symbol (str "map->" ~rec-name#))) ~proto#) ;exemplar
                     :type-proto ~type-proto#
                     :primary    (vec (map keyword '~primary))
                     :extras     ~extras#}
                    {:type? true}))



           ~(symbol rec-name#))))))

(defn- find-entity
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


(defn- get-key-info
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
  "Carry type information in the meta data"
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

(defn make-key
  "Make a key value. Returns a map comprising only the map keys
  for the given domain key name. Any missing fields are
  nil (or explicit default value); additional
  fields are ignored."
  [entity key-name key-val]
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
        (t/merge key-val))))

(defn- primary-key-to-meta
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
           (:entity cur-meta)
           :primary
           instance)))))

(defn get-alias
  "Return a keyword that can be used to associate the
  instance in a map. This will be either its name or
  any alias that was specified when defined."
  [instance]
  (let [entity (cond
                 (keyword? instance) (find-entity instance)
                 (and (:type? (meta instance)) (:proto instance)) instance
                 :else (find-entity (:entity (meta instance))))]
    (or (-> entity
            :extras
            :alias)
        (:name entity))))

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
         (t/merge ent-val)))))

(defn get-primary-key
  "Return the instance's primary key from its meta data.
  If not found, throws"
  [instance]
  (or (-> instance
          meta
          :primary)
      (throw (ex-info (str "Primary key not set") {:instance instance
                                                   :meta     (meta instance)}))))
(defn read-entity
  "Read the given entity, applying the key value"
  ([key-val]
   (let [key-meta (meta key-val)]
     (read-entity (:entity key-meta) (:key key-meta) key-val)))
  ([entity-name key-val]
   (read-entity entity-name
                (-> key-val
                    meta
                    :key)
                key-val))
  ([entity-name key-name key-val]
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

(defn- key-value?
  "Returns the key name as truthy if the argument is a key value, that
  is, as returned from (make-key ...); false otherwise"
  ([key-val] (key-value? key-val :key))
  ([key-val meta-item]
   (let [m (meta key-val)]
     (if (every? #(contains? m %) [:entity :key :proto :unique?])
       (meta-item m)
       false))))

(defn- unique-key?
  "Returns the key name as truthy if the argument is a key value, that
  is, as returned from (make-key ...); false otherwise"
  [key-val]
  (let [m (meta key-val)]
    (if (every? #(contains? m %) [:entity :key :proto])
      (:key m)
      false)))

(defn aggregate
  "Aggregate from a (possibly empty) data structure to the
  target entity reference. Provides for the common cases of
  structure building.

  The structure root is always a map. Non-unique keys result in
  a vector whose entries are themselves maps. The key of a vector
  result is always supplied by the caller as the option :set-name
  By default instances are held as their unqualified type name or any
  alias that was defined in the declaration.

   - data : the structure being built. Must be a map and always the
            root even in successive calls to this function.
   - opts : options as follows:
     :from <path>   a path to the thing being aggregated from.
                    This is only absent when seeding the structure
                    with its initial value (or vector of values, in the
                    case of a non-unique key). When passing through
                    a vector indicate this using '>'
     :to <type-ref> mandatory - the type being joined in the structure.
     :key-val       when :from is present, by default it will be used
                    as the value for the target type's :primary key, however
                    this behaviour is overridden by key-val, which may
                    be either the return value of make-key or a vector tuple
                    of [key-name key-value], a keyword identifying a
                    known key or a function (see below).
     :instance-name the map key to use when placing single instances in the
                    structure. This applies whether the key being applied
                    is unique or not. The name will be used in all
                    map children housing each instance.
     :set-name      the map key for the vector returned by non-unique keys
                    when placed in the parent map.
     :transform-fn  A function to perform the transform. This must accept
                    four arguments: [parent from f-opts cur] respectively
                    the structure parent node, the instance being joined
                    from, the options (see below) and any current
                    value (likely nil unless the path identifies a
                    pre-existing element in the structure). The returned
                    value will be placed in the structure.
  If key-val is a function it must accept three arguments. These are
   - parent : the parent node in the structure
   - from   : the value being aggregated from
   - f-opts : a map containing :key <key-name>, :key-val,
                               :entity <the type being joined>,
                               :set-name and :instance-name"
  [data & opts]
  (let [{:keys [from to key-val instance-name set-name transform-fn]} opts
        to-entity (find-entity to)
        key-name (or (and (vector? key-val) (key-val 0))
                     (key-value? key-val)
                     (and (keyword? key-val)
                          key-val)
                     :primary)
        key-info (get-key-info to-entity key-name)
        unique-key? (:unique? key-info)
        instance-name (or instance-name (get-alias to-entity))
        set-name (or set-name unique-key? (throw
                                            (ex-info "non-unique key requires a set-name"
                                                     {:entity (:name to-entity)
                                                      :key key-name})))
        set-name (if (true? set-name) nil set-name)
        path-len (count from)
        f-opts {:key key-name
                :key-val key-val
                :entity to
                :set-name set-name
                :instance-name instance-name}
        from (or (and (vector? from)
                      (>= path-len 3)
                      (loop [from from
                             cur  (first from)
                             idx  0
                             last-vec (- path-len 2)
                             last-elem (+ last-vec 1)
                             result []]
                        (if cur
                          (cond
                            (and (= cur >)
                                 (= idx last-vec))
                            (recur (rest from)
                                   (second from)
                                   (inc idx)
                                   last-vec
                                   last-elem
                                   (concat result [sp/ALL sp/VAL]))
                            (= idx last-elem)
                            (recur (rest from)
                                   (second from)
                                   (inc idx)
                                   last-vec
                                   last-elem
                                   (concat result [(sp/collect-one cur)
                                                   (sp/putval f-opts)
                                                   (if unique-key?
                                                     instance-name
                                                     set-name)]))
                            :else
                            (recur (rest from)
                                   (second from)
                                   (inc idx)
                                   last-vec
                                   last-elem
                                   (conj result cur)))
                          result)))
                 (and (= path-len 0)
                      [(sp/putval nil) ; parent
                       (sp/putval nil) ; from
                       (sp/putval f-opts)
                       (if unique-key?
                         instance-name
                         set-name)])
                 (and (vector? from)
                      (= path-len 1)
                      [sp/VAL                    ; parent
                       (sp/collect-one (from 0)) ; from
                       (sp/putval f-opts)
                       (if unique-key?
                         instance-name
                         set-name)])
                 (throw (ex-info "Illegal 'from' argument" {:arg from})))]
    (sp/transform
      from
      (if transform-fn
        transform-fn
        (fn [parent from f-opts cur]
          (let [l-key-val
            (cond
              (nil? key-val)
              (make-key to :primary from)

              (keyword? key-val)
              (make-key to key-val from)

              (vector? key-val)
              (let [[key-name key-val] key-val]
                (make-key to key-name key-val))

              (key-value? key-val)
              (let [key-meta (meta key-val)]
                (if (= to
                       (:entity key-meta))
                  key-val
                  (make-key to key-name key-val)))

              (map? key-val)
              (make-key to key-name key-val)

              (fn? key-val)
              (key-val parent from f-opts)
              :else
              (throw (ex-info "Illegal arguments: " {:key-val key-val
                                                     :from    from
                                                     :target  to})))]
            (if (key-value? l-key-val :unique?)
              (read-entity l-key-val)
              (into [] (map #(assoc {}
                               instance-name %)
                            (read-entity l-key-val))))))) data)))
