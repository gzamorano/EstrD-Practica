module PriorityQueue 
    (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
where

data PriorityQueue a = PQ [a]

-- Propósito: devuelve una priority queue vacía.
emptyPQ :: PriorityQueue a

-- Propósito: indica si la priority queue está vacía.
isEmptyPQ :: PriorityQueue a -> Bool

-- Propósito: inserta un elemento en la priority queue.
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a

-- Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
-- Precondición: parcial en caso de priority queue vacía.
findMinPQ :: Ord a => PriorityQueue a -> a

-- Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
-- Precondición: parcial en caso de priority queue vacía.
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a

-- O(1)
emptyPQ = PQ []

-- O(1)
isEmptyPQ (PQ xs) = null xs

-- O()
insertPQ x (PQ xs) = PQ (x:xs) 

-- O()
findMinPQ (PQ xs) = minimum xs

-- O()
deleteMinPQ (PQ xs) = PQ (sinElMinimo xs)

sinElMinimo :: Ord a => [a] -> [a]
--PRECOND: la lista no es bacia
sinElMinimo xs = borrar (minimum xs) xs

borrar x []     = []
borrar x (y:ys) = if x==y then ys else y:borrar x ys 


