(ns entity.aggregate
  (:require [com.rpl.specter :as sp]
            [entity.core :refer [find-entity get-alias get-key-info make-key read-entity]]
            [entity.protocol :refer [read-fn]]))

(defn- key-value?
  "Returns the key name or other metadata marker as truthy if the
  argument is a key value, that is, as returned from (make-key ...)
  false otherwise"
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

(defn- merge-primary
  "Merge the given sequences of instances by their primary key"
  [instance-name current to-add]
  (let [as-map-f (fn [x] (reduce #(assoc %1
                                    (or (-> %2
                                            instance-name
                                            meta
                                            :primary)
                                        (throw (ex-info "Primary key unavailable" {})))
                                    %2)
                                 {}
                                 x))
        cur-m (as-map-f current)
        add-m (as-map-f to-add)]
    (into [] (vals (merge add-m cur-m)))))

(defmethod read-fn :default
  [_ key-val & args]
  (apply read-entity key-val args))

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
     :from <path>   a vector path to the thing being aggregated from.
                    This is only absent when seeding the structure
                    with its initial value (or vector of values, in the
                    case of a non-unique key). When passing through
                    a vector in the structure indicate this using '>'
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
                    map children housing each instance. Optional and if
                    absent the unqualified name or any alias is used.
     :set-name      the map key for the vector returned by non-unique keys
                    when placed in the parent map. Mandatory when a non-unique
                    key is being used, otherwise ignored.
     :merge         Any existing value will always be replaced by a new one.
                    In the case of a non-unique key, an existing vector
                    will be replaced unless this option specifies
                    either :primary or a function. The option of :primary
                    will merge current and new values into the result
                    vector by the primary key set. A function must accept
                    three arguments, the instance-name, current and new
                    vectors, and return a vector containing the required merge.
     :must-join     If true, when aggregating to a vector, the map child will
                    be removed for any instances that don't join with the
                    target. Otherwise the vector entry remains with a
                    nil child where there is no join.
     :for-each      A function called after the aggregation. Will be passed
                    the parent node
  If key-val is a function it must accept three arguments. These are
   - parent : the parent node in the structure
   - from   : the value being aggregated from
   - f-opts : a map containing :key <key-name>, :key-val,
                               :entity <the type being joined>,
                               :set-name and :instance-name"
  [data & opts]
  (or (map? data) (throw (ex-info "data must be a map" {})))
  (let [{:keys [from
                to
                key-val
                instance-name
                set-name
                for-each
                merge
                must-join
                read-f]} opts
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
                                                      :key    key-name})))
        set-name (if (true? set-name) nil set-name)
        path-len (count from)
        f-opts {:key           key-name
                :key-val       key-val
                :entity        to
                :set-name      set-name
                :instance-name instance-name}
        path (or (and (vector? from)
                      (>= path-len 3)
                      (loop [from from
                             cur (first from)
                             idx 0
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
                                   (vec (concat result [sp/ALL sp/VAL])))
                            (= idx last-elem)
                            (recur (rest from)
                                   (second from)
                                   (inc idx)
                                   last-vec
                                   last-elem
                                   (vec (concat result (reduce conj [(sp/collect-one cur)
                                                                     (sp/putval f-opts)]
                                                               (if unique-key?
                                                                 (if (vector? instance-name) instance-name [instance-name])
                                                                 (if (vector? set-name) set-name [set-name]))))))
                            :else
                            (recur (rest from)
                                   (second from)
                                   (inc idx)
                                   last-vec
                                   last-elem
                                   (conj result cur)))
                          result)))
                 (and (nil? from)
                      (reduce conj [(sp/putval nil)         ; parent
                                    (sp/putval nil)         ; from
                                    (sp/putval f-opts)]
                              (if unique-key?
                                (if (vector? instance-name) instance-name [instance-name])
                                (if (vector? set-name) set-name [set-name]))))
                 (and (vector? from)
                      (= path-len 1)
                      (reduce conj [sp/VAL                               ; parent
                                    (sp/collect-one (from 0))            ; from
                                    (sp/putval f-opts)]
                              (if unique-key?
                                (if (vector? instance-name) instance-name [instance-name])
                                (if (vector? set-name) set-name [set-name]))))
                 (throw (ex-info "Illegal 'from' argument" {:arg from})))]
    (cond->>
      (sp/transform
        path
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
              (read-fn read-f l-key-val)
              (let [result (into [] (map #(assoc {}
                                            instance-name %)
                                         (read-fn read-f l-key-val)))]
                (cond
                  (nil? merge) result
                  (= :primary merge) (merge-primary instance-name cur result)
                  (fn? merge) (merge instance-name cur result)
                  :else (throw (ex-info "Illegal merge-fn" {:arg merge})))
                ))))
        data)
      must-join
      (sp/setval
        (assoc path
          (dec (count path))
          #(nil? (instance-name %)))
        sp/NONE)
      for-each
      (sp/transform
        (cond
          (nil? from)
          [(last path) sp/ALL]

          (= path-len 1)
          []

          :else
          (sp/setval [sp/ALL #(= > %)] sp/ALL (pop from)))
        for-each))))
