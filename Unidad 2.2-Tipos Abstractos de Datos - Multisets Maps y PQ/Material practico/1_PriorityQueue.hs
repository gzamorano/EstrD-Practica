-- Ejercicio 2 con PQ como estructura auxiliar. ver el costo
import PriorityQueue1

-- O(N^2) donde N es la cantidad de elementos de la lista, y asumiendo que listToPQ es de costo
--      cuadrático, al igual que pqToList.
-- Propósito: dada una lista la ordena de menor a mayor utilizando una PQ como estructura auxiliar.
pqSort :: Ord a => [a] -> [a] 
pqSort xs = pqToList (listToPQ xs)

-- O(N^2) donde N es la cantidad de elementos de la lista, sobre la que se realiza RE, 
--      y asumiendo que insertPQ es lineal.
listToPQ :: Ord a => [a] -> PriorityQueue a
listToPQ []     = emptyPQ
listToPQ (x:xs) = insertPQ x (listToPQ xs)

-- O(n^2) donde n es la cantidad de elementos de la PQ y asumiendo que deleteMinPQ es lineal.
pqToList  :: Ord a => PriorityQueue a -> [a]
pqToList pq = if isEmptyPQ pq
                then []
                else findMinPQ pq : pqToList (deleteMinPQ pq)