-- FruitSupplier

-- :snip select-stmt
SELECT
 FS.Fruit          AS "Fruit",
 FS.Supplier       AS "Supplier",
 FS.PricePerKg     AS "PricePerKg",
 FS.LastOrdered    AS "LastOrdered"
FROM FruitSupplier FS

-- :name primary :? :1
:snip:select-stmt
WHERE FS.Fruit = :Fruit
AND   FS.Supplier = :Supplier

-- :name write :! :n
MERGE INTO FruitSupplier
VALUES (:Fruit, :Supplier, :PricePerKg, :LastOrdered)

-- :name delete :! :n
DELETE FROM FruitSupplier
WHERE Fruit = :Fruit
AND   Supplier = :Supplier

-- :name by-fruit :? :*
:snip:select-stmt
WHERE FS.Fruit = :Fruit

-- :name filter :? :*
:snip:select-stmt
, Fruit F,
Supplier S
WHERE F.Fruit = FS.Fruit
AND   S.Supplier = FS.Supplier
AND (FS.Fruit = :Fruit OR :Fruit IS NULL)
AND (FS.Supplier = :Supplier OR :Supplier IS NULL)
AND (F.ShelfLife >= :MinShelfLife OR :MinShelfLife IS NULL)
AND (F.ShelfLife <= :MaxShelfLife OR :MaxShelfLife IS NULL)
AND (F.Active = :FruitActive OR :FruitActive IS NULL)
AND (S.Active = :SupplierActive OR :SupplierActive IS NULL)
AND (F.Freezable = :Freezable OR :Freezable IS NULL)
AND (FS.LastOrdered >= :FromDate OR :FromDate IS NULL)
AND (FS.LastOrdered <= :ToDate OR :ToDate IS NULL)

-- :name all :? :*
:snip:select-stmt

