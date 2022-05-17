-- Costo de heapsort suponiendo que ahora el usuario utiliza una 
-- Priority Queue con costos logarítmicos de inserción y borrado.

-- ME QUEDARÍA COSTO CUADRÁTICO, NO CAMBIA NADA?


-- O(n^2) siendo n el tamaño de la lista, asumiendo que listToPQ es de costo cuadrático.
-- Propósito: dada una lista la ordena de menor a mayor utilizando una PQ como estructura auxiliar.
pqSort :: Ord a => [a] -> [a] 
pqSort xs = pqToList (listToPQ xs)


-- O(n log n) donde N es la cantidad de elementos de la lista, sobre la que se realiza RE, 
-- y asumiendo que insertPQ es de costo log n.     
listToPQ :: Ord a => [a] -> PriorityQueue a
listToPQ []     = emptyPQ
listToPQ (x:xs) = insertPQ x (listToPQ xs)

-- O(n^2) donde n es la cantidad de elementos de la PQ y asumiendo que:
-- findMinPQ es de costo lineal.
-- findMinPQ usa minimum por eso queda con costo lineal, se puede mejorar??
pqToList  :: Ord a => PriorityQueue a -> [a]
pqToList pq = if isEmptyPQ pq
                then []
                else findMinPQ pq : pqToList (deleteMinPQ pq)