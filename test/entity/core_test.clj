(ns entity.core-test
  (:require [clojure.test :refer :all]
            [entity.core :refer :all]
            [entity.aggregate :refer [aggregate]]
            [entity.sql.hug :as sql]
            [typeops.assign :as typeops]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc])
  (:import (java.text SimpleDateFormat)))

(def now (java.sql.Timestamp. 0))

(def fmt (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss"))

(defn str-to-sql-date
  [s]
  (java.sql.Timestamp. (.getTime (.parse fmt "2017-06-02T12:03:42"))))


(def fruit-db-spec {:jdbc-url "jdbc:h2:./test.db"})
(def ^:dynamic *fruit-db* (sql/connect! fruit-db-spec))

(def fruit-db-opts {:entity-opts
                    {:server-type :h2}})

; Scalar types
(defscalar :foo/StringId "")
(defscalar :foo/NumDays 0)
(defscalar :foo/LongVal 0)
(defscalar :foo/LongName "")
(defscalar :foo/Money 0.00M)
(defscalar :foo/Date now)
(defscalar :foo/DateTime now)
(defscalar :foo/AddressLine "")

; Enum types
(defenum :foo/Freezable {:y "Y"
                         :n "N"} :y)

(defenum :foo/Active {:y 1
                      :n 0} :y)

(defentity :foo/Fruit
           [Fruit          :foo/StringId
            Description    :foo/LongName
            ShelfLife      :foo/NumDays = 1
            Active         (enum-val :foo/Active :y)
            Freezable      (enum-val :foo/Freezable :n)]
           [Fruit]
           :keys {:all         {:unique? false
                                :cached? false
                                :fields  []}
                  :by-active   {:unique? false
                                :cached? true
                                :fields  [Active]}
                  :by-supplier {:unique? false
                                :cached? false
                                :fields  [:foo/Supplier.Supplier]}
                  :filter      {:unique? false
                                :cached? false
                                :fields  [Fruit
                                          Active :as FruitActive
                                          Freezable
                                          ShelfLife :as MinShelfLife = 7
                                          ShelfLife :as MaxShelfLife]}}
           :io (sql/bind-connection *fruit-db* "sql/fruit.sql" fruit-db-opts)
           :create (fn [instance] (comment stuff))
           :mutate (fn [old new] (comment stuff))
           :join (fn [instance] (comment stuff))
           :destroy (fn [instance] (comment stuff))
           :alias :Fruit)

(defentity :foo/Nutrition
           [Fruit       :foo/StringId
            KCalPer100g :foo/LongVal
            Fat         :foo/LongVal
            Salt        :foo/LongVal]
           [Fruit]
           :io (sql/bind-connection *fruit-db* "sql/nutrition.sql" fruit-db-opts)
           :alias :Nutrition)

(defentity :foo/FruitSupplier
           [Fruit       :foo/StringId
            Supplier    :foo/StringId
            PricePerKg  :foo/Money
            LastOrdered now]
           [Fruit Supplier]
           :keys {:by-fruit {:unique? false
                             :cached true
                             :fields [Fruit]}
                  :filter {:unique? false
                           :cached? false
                           :fields  [Fruit
                                     Supplier
                                     :foo/Fruit.Active :as FruitActive
                                     :foo/Supplier.Active :as :SupplierActive
                                     LastOrdered :as FromDate
                                     LastOrdered :as ToDate
                                     :foo/Fruit.Freezable
                                     :foo/Fruit.ShelfLife :as MinShelfLife
                                     :foo/Fruit.ShelfLife :as MaxShelfLife]}
                  :all          {:unique? false
                                 :cached? false
                                 :fields  []}}
           :io (sql/bind-connection *fruit-db* "sql/fruitsupplier.sql" fruit-db-opts))

(defentity :foo/Supplier
           [Supplier    :foo/StringId
            Active      (enum-val :foo/Active :y)
            Address1    :foo/AddressLine
            Address2    :foo/AddressLine]
           [Supplier]
           :keys {:by-fruit {:unique? false
                             :cached? false
                             :fields [:foo/Fruit.Fruit]}
                  :all {:unique? false
                        :cached? false
                        :fields  []}}
           :io (sql/bind-connection *fruit-db* "sql/supplier.sql" fruit-db-opts))

(def strawberry (new-instance
                  :foo/Fruit
                  {:Fruit "Strawberry"
                   :Description "Soft Summer Fruit"
                   :ShelfLife 14
                   :Active (enum-val :foo/Active :y)
                   :Freezable (enum-val :foo/Freezable :y)}))

(def banana (new-instance
              :foo/Fruit
              {:Fruit       "Banana"
               :Description "Yellow and not straight"
               :ShelfLife   21
               :Active      (enum-val :foo/Active :y)
               :Freezable   (enum-val :foo/Freezable :n)}))

(def pineapple (new-instance
                 :foo/Fruit
                 {:Fruit       "Pineapple"
                  :Description "Edible Bromeliad"
                  :ShelfLife   46
                  :Active      (enum-val :foo/Active :n)                            ; Out of season
                  :Freezable   (enum-val :foo/Freezable :n)}))

(def strawberry-nutrition (new-instance
                            :foo/Nutrition
                            {:Fruit       "Strawberry"
                             :KCalPer100g 40
                             :Fat         0
                             :Salt        0}))

(def banana-nutrition (new-instance
                        :foo/Nutrition
                        {:Fruit       "Banana"
                         :KCalPer100g 50
                         :Fat         1
                         :Salt        0}))

(def pineapple-nutrition (new-instance
                           :foo/Nutrition
                           {:Fruit       "Pineapple"
                            :KCalPer100g 45
                            :Fat         0
                            :Salt        1}))

; bad-apple is not an instance created with new-instance
; As such it's not writeable to the db.
(def bad-apple {:Fruit       "BadApple"
                :Description "Always one"
                :ShelfLife   0
                :Active      (enum-val :foo/Active :y)
                :Freezable   (enum-val :foo/Freezable :n)})

(def strawberries-from-kent (new-instance
                              :foo/FruitSupplier
                              {:Fruit       "Strawberry"
                               :Supplier    "Kent Fruits"
                               :PricePerKg  2.75M
                               :LastOrdered (str-to-sql-date "2017-06-02T12:03:42")}))

(def strawberries-from-sussex (new-instance
                                :foo/FruitSupplier
                                {:Fruit       "Strawberry"
                                 :Supplier    "Sussex Fruits"
                                 :PricePerKg  2.79M
                                 :LastOrdered (str-to-sql-date "2017-06-02T13:07:31")}))

(def pineapples-from-sussex (new-instance
                                :foo/FruitSupplier
                                {:Fruit       "Pineapple"
                                 :Supplier    "Sussex Fruits"
                                 :PricePerKg  3.49M
                                 :LastOrdered (str-to-sql-date "2017-06-01T23:07:31")}))

(def fruit-supplier-mappings [strawberries-from-kent
                              strawberries-from-sussex
                              pineapples-from-sussex])

(def kent-fruits (new-instance
                   :foo/Supplier
                   {:Supplier "Kent Fruits"
                    :Active   (enum-val :foo/Active :y)
                    :Address1 "The Fruit Farm"
                    :Address2 "Deepest Kent"}))

(def sussex-fruits (new-instance
                     :foo/Supplier
                     {:Supplier "Sussex Fruits"
                      :Active   (enum-val :foo/Active :y)
                      :Address1 "All Fruits"
                      :Address2 "South Downs"}))

(defn disconnect
  []
  (sql/disconnect! *fruit-db*))

(defn delete-test-db []
  (io/delete-file "test.db.mv.db" true)
  (io/delete-file "test.db.trace.db" true))

(defn create-fruits-table []
  (jdbc/db-do-commands
    *fruit-db*
    false
    ["DROP TABLE Fruit IF EXISTS;"
     (jdbc/create-table-ddl
       :Fruit
       [[:Fruit "VARCHAR(32)" "PRIMARY KEY"]
        [:Description "VARCHAR(32)"]
        [:ShelfLife :int]
        [:Active :int]
        [:Freezable "CHAR(1)"]])]))

(defn create-nutrition-table []
  (jdbc/db-do-commands
    *fruit-db*
    false
    ["DROP TABLE Nutrition IF EXISTS;"
     (jdbc/create-table-ddl
       :Nutrition
       [[:Fruit "VARCHAR(32)" "PRIMARY KEY"]
        [:KCalPer100g :int]
        [:Fat :int]
        [:Salt :int]])]))

(defn create-fruits-supplier-table []
  (jdbc/db-do-commands
    *fruit-db*
    false
    ["DROP TABLE FruitSupplier IF EXISTS;"
     (jdbc/create-table-ddl
       :FruitSupplier
       [[:Fruit "VARCHAR(32) NOT NULL"]
        [:Supplier "VARCHAR(32) NOT NULL"]
        [:PricePerKg "DECIMAL(20,2)"]
        [:LastOrdered "DATETIME"]])
     "ALTER TABLE FruitSupplier ADD PRIMARY KEY (Fruit, Supplier);"]))

(defn create-supplier-table []
  (jdbc/db-do-commands
    *fruit-db*
    false
    ["DROP TABLE Supplier IF EXISTS;"
     (jdbc/create-table-ddl
       :Supplier
       [[:Supplier "VARCHAR(32) PRIMARY KEY"]
        [:Active :int]
        [:Address1 "VARCHAR(32)"]
        [:Address2 "VARCHAR(32)"]])]))

(deftest write
  (create-fruits-table)
  (testing "write two instances"
    (is (= 2
           (+ (write-instance strawberry)
              (write-instance banana))))))

(deftest read-correct-type
  (create-fruits-table)
  (write-instance strawberry)
  (write-instance banana)
  (write-instance pineapple)
  (create-nutrition-table)
  (write-instance strawberry-nutrition)
  (write-instance banana-nutrition)
  (write-instance pineapple-nutrition)
  (is (= Fruit
         (type (read-entity (make-key
                              :foo/Fruit
                              :primary
                              {:Fruit "Strawberry"}))))))

(deftest read-multi
  (create-fruits-table)
  (write-instance strawberry)
  (write-instance banana)
  (is (= #{banana strawberry}
         (into #{} (read-entity (make-key
                                  :foo/Fruit
                                  :all
                                  {}))))))

(deftest write
  (create-fruits-table)
  (write-instance strawberry)
  (let [mod-strawb (typeops/assign strawberry :ShelfLife 18)
        write-res (write-instance mod-strawb)
        read-strawb (read-entity (make-key :foo/Fruit :primary strawberry))]
    (is (= 1 write-res))
    (is (= mod-strawb read-strawb))))

(deftest delete
  (create-fruits-table)
  (let [write-res (write-instance strawberry)
        delete-res (delete-instance strawberry)
        read-strawb (read-entity (make-key :foo/Fruit :primary strawberry))]
    (is (= 1 write-res))
    (is (= 1 delete-res))
    (is (nil? read-strawb))))

(deftest transaction-abort
  (do
    (create-fruits-table)
    (write-instance strawberry))

  (try
    (sql/with-transaction
      [*fruit-db*]
      (is
        (= 1
           (write-instance (typeops/assign strawberry :ShelfLife 18))))
      (write-instance bad-apple))
    (catch Exception _))

  (is
    (= strawberry (read-entity (make-key :foo/Fruit :primary strawberry)))))

(deftest filter-key-test
  (is (do
        (create-fruits-table)
        (create-fruits-supplier-table)
        (create-supplier-table)
        (write-instance kent-fruits)
        (write-instance sussex-fruits)
        (write-instance strawberry)
        (write-instance pineapple)
        (write-instance banana)
        (doall (map #(write-instance %1) fruit-supplier-mappings))
      (= #{strawberries-from-kent strawberries-from-sussex}
         (into #{} (read-entity (make-key
                        :foo/FruitSupplier
                        :filter
                        {:Fruit          nil,
                         :Supplier       nil ;"Kent Fruits",
                         :FruitActive    nil,
                         :Freezable      nil,
                         :MinShelfLife   7,
                         :MaxShelfLife   20,
                         :SupplierActive nil,
                         :FromDate       nil,
                         :ToDate         nil})))))))

(deftest aggregate-primary-test
  (create-fruits-table)
  (create-nutrition-table)
  (write-instance strawberry)
  (write-instance banana)
  (write-instance pineapple)
  (write-instance strawberry-nutrition)
  (write-instance banana-nutrition)
  (write-instance pineapple-nutrition)

  (is (= {:fruits [{:Fruit     strawberry
                    :Nutrition strawberry-nutrition}
                   {:Fruit    banana
                    :Nutrition banana-nutrition}
                   {:Fruit pineapple
                    :Nutrition pineapple-nutrition}]}
         (-> {}
             (aggregate :to :foo/Fruit :key-val [:all {}] :set-name :fruits)
             (aggregate :to :foo/Nutrition :from [:fruits > :Fruit])))))

(deftest aggregate-big-test
  (create-fruits-table)
  (create-fruits-supplier-table)
  (create-supplier-table)
  (write-instance kent-fruits)
  (write-instance sussex-fruits)
  (write-instance strawberry)
  (write-instance pineapple)
  (write-instance banana)
  (doall (map #(write-instance %1) fruit-supplier-mappings))
  (is (= {:Fruit strawberry,
          :suppliers [{:Supplier kent-fruits,
                       :fruits [{:Fruit strawberry}]}
                      {:Supplier sussex-fruits,
                       :fruits [{:Fruit strawberry}
                                {:Fruit pineapple}]}],
          :Nutrition strawberry-nutrition}
         (-> {}
             (aggregate :to :foo/Fruit :key-val {:Fruit "Strawberry"})
             (aggregate :to :foo/Supplier :from [:Fruit] :instance-name :Supplier :set-name :suppliers :key-val :by-fruit)
             (aggregate :to :foo/Nutrition :from [:Fruit])
             (aggregate :to :foo/Fruit :from [:suppliers > :Supplier] :key-val :by-supplier :set-name :fruits)))))

(deftest aggregate-must-join-test
  (create-fruits-table)
  (create-nutrition-table)
  (write-instance strawberry)
  (write-instance pineapple)
  (write-instance banana)
  (write-instance strawberry-nutrition)
  (write-instance banana-nutrition)
  (doall (map #(write-instance %1) fruit-supplier-mappings))
  (is (= {:fruits [{:Fruit strawberry,
                    :Nutrition strawberry-nutrition}
                   {:Fruit banana,
                    :Nutrition banana-nutrition}]}
                   (-> {}
                       (aggregate :set-name :fruits :to :foo/Fruit :key-val (make-key :foo/Fruit :filter {}))
                       (aggregate :from [:fruits > :Fruit] :to :foo/Nutrition :must-join true)))))

(deftest aggregate-merge-extra-test
  (create-fruits-table)
  (create-fruits-supplier-table)
  (create-supplier-table)
  (write-instance kent-fruits)
  (write-instance sussex-fruits)
  (write-instance strawberry)
  (write-instance pineapple)
  (write-instance banana)
  (doall (map #(write-instance %1) fruit-supplier-mappings))
  (is (= {:fruits [{:Fruit strawberry}
                   {:Fruit banana}
                   {:Fruit pineapple}
                   ]}
         (-> {}
             (aggregate :set-name :fruits :to :foo/Fruit :key-val (make-key :foo/Fruit :filter {:FruitActive 0}))
             (aggregate :merge :primary :set-name :fruits :to :foo/Fruit :key-val (make-key :foo/Fruit :filter {:FruitActive 1}))
             )))
  (is (= {:Fruit strawberry,
         :suppliers [{:Supplier kent-fruits,
                      :fruits [{:Fruit strawberry}],
                      :extra "hello"}
                     {:Supplier sussex-fruits,
                      :fruits [{:Fruit strawberry}
                               {:Fruit pineapple}],
                      :extra "hello"}],
         :Nutrition strawberry-nutrition,
         :extra "foo"})
      (-> {}
          (aggregate :to :foo/Fruit :key-val {:Fruit "Strawberry"})
          (aggregate :to :foo/Supplier :from [:Fruit] :instance-name :Supplier :set-name :suppliers :key-val :by-fruit)
          (aggregate :to :foo/Nutrition :from [:Fruit] :for-each (fn bar [v] (assoc v :extra "foo")))
          (aggregate :to :foo/Fruit :for-each (fn foo [v] (assoc v :extra "hello")) :from [:suppliers > :Supplier] :key-val :by-supplier :set-name :fruits))))


