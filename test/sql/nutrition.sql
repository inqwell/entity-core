-- Nutrition

-- :snip select-stmt
SELECT
 N.Fruit          AS "Fruit",
 N.KCalPer100g    AS "KCalPer100g",
 N.Fat            AS "Fat",
 N.Salt           AS "Salt"
FROM Nutrition N

-- :name primary :? :1
:snip:select-stmt
WHERE N.Fruit = :Fruit

-- :name write :! :n
MERGE INTO Nutrition
VALUES (:Fruit, :KCalPer100g, :Fat, :Salt)

-- :name delete :! :n
DELETE FROM Nutrition
WHERE Fruit = :Fruit
