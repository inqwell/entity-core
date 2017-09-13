(ns entity.protocol)

(defprotocol IO
  "Manage an entity in its persistence mechanism"
  (read-key
    [io key-val]
    [io key-name key-val])
  (write-val [io entity-val])
  (delete-val [io entity-val]))
