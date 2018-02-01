(ns entity.protocol)

(defprotocol IO
  "Manage an entity in its persistence mechanism"
  (read-key
    [io key-val]
    [io key-name key-val])
  (write-val [io entity-val])
  (delete-val [io entity-val])
  (as-transaction [f]))

(defmulti read-fn
          "Read instances from their persistent store"
          (fn [read-type key-val & args]
            read-type))

