import Set1

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

-- O(n * b(m))
-- donde n es el tamaño de la lista y sobre la cual se realiza recursión estructural
-- donde b es el costo de belongs y m es el tamaño del set

-- [1..1000] * {1..10} -> 100000 operaciones
-- en losQuePertenecen (usa belongs)


-- 2. Como usuario del tipo abstracto Set implementar las siguientes funciones:
--losQuePertenecen :: Eq a => [a] -> Set a -> [a]
--Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
--al conjunto.


--sinRepetidos :: Eq a => [a] -> [a]
--Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
-- [1,3,1,5,3,7,1] -> Set -> [1,3,5,7] 
--                  setToList



--unirTodos :: Eq a => Tree (Set a) -> Set a
--Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
--del arbol.
