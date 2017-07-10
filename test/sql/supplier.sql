-- Supplier

-- :snip select-stmt
SELECT
 S.Supplier       AS "Supplier",
/*~
(if (= (:server-type params) :h2)
 "S.Active"
 "S.Activo")
~*/ AS "Active",
 S.Address1    AS "Address1",
 S.Address2    AS "Address2"
FROM Supplier S

-- :name primary :? :1
:snip:select-stmt
WHERE S.Supplier = :Supplier

-- :name write :! :n
MERGE INTO Supplier
VALUES (:Supplier, :Active, :Address1, :Address2)

-- :name delete :! :n
DELETE FROM Supplier
WHERE Supplier = :Supplier

-- :name by-fruit :? :*
:snip:select-stmt
, FruitSupplier FS
WHERE S.Supplier = FS.Supplier
AND FS.Fruit = :Fruit

-- :name all :? :*
:snip:select-stmt
