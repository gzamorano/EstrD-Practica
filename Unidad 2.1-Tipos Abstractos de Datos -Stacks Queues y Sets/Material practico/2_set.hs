-- import Set1
import Set2


data Tree a = EmptyT | NodeT a (Tree a) (Tree a)



-- [1..1000] * {1..10} -> 100000 operaciones
--losQuePertenecen (usa belongs)


-- 2.2 Como usuario del tipo abstracto Set implementar las siguientes funciones:

mySet  = addS 5 $ addS 7 $ addS 10 emptyS
mySet2 = addS 8 $ addS 1 $ addS 10 emptyS

-- O(n * b(m)) donde n es el tamaño de la lista y sobre la cual se realiza recursión estructural
--             donde b es el costo de belongs y m es el tamaño del set
-- Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
-- al conjunto(con repetidos en la lista final).
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen []     s = []
losQuePertenecen (x:xs) s = 
    if belongS x s
        then x : losQuePertenecen xs s
        else losQuePertenecen xs s 

-- O(n) donde n es el tamaño de la lista 
--Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
-- [1,3,1,5,3,7,1] -> Set -> [1,3,5,7] 
--                  setToList
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (listToSet xs)

-- O(n) donde n es el tamaño de la lista y sobre la cual se hace recursión estructural
listToSet :: Eq a => [a] -> Set a
listToSet []     = emptyS
listToSet (x:xs) = addS x (listToSet xs)

-- O()
--Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos del arbol.
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT          = emptyS
unirTodos (NodeT s t1 t2) = 
    unionS s 
           (unionS (unirTodos t1) 
                   (unirTodos t2))


treeS = NodeT mySet 
                  EmptyT
                  (NodeT mySet2 EmptyT EmptyT)



-- funcion que creo yo
agregarMuchos :: Eq a => [a] -> Set a -> Set a
agregarMuchos []     s = s
agregarMuchos (x:xs) s = unionS (addS x s) 
                                (agregarMuchos xs s)