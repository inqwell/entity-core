-- Supplier

-- :snip select-stmt
SELECT
 F.Supplier       AS "Supplier",
/*~
(if (= (:server-type params) :h2)
 "F.Active"
 "F.Activo")
~*/ AS "Active",
 F.Address1    AS "Address1",
 F.Address2    AS "Address2"
FROM Supplier F

-- :name primary :? :1
:snip:select-stmt
WHERE F.Supplier = :Supplier

-- :name write :! :n
MERGE INTO Supplier
VALUES (:Supplier, :Active, :Address1, :Address2)

-- :name delete :! :n
DELETE FROM Supplier
WHERE F.Supplier = :Supplier

-- :name all :? :*
:snip:select-stmt
