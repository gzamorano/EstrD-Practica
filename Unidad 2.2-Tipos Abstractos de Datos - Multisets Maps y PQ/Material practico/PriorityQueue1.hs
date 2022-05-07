-- Ejercicio 1
module PriorityQueue1 
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

-- O(1) arma una PQ con su constructor y una lista vacía.
emptyPQ = PQ []

-- O(1) asumiendo que null es operación de costo constante.
isEmptyPQ (PQ xs) = null xs

-- O(n) siendo n la cantidad de elementos de la PQ y asumiendo que cons es de costo constante.
insertPQ x (PQ xs) = PQ (x:xs) 

-- O(n) siendo n la cantidad de elementos de la PQ y asumiendo que minimum es lineal.
findMinPQ (PQ xs) = minimum xs

-- O(n) donde n es la cantidad de elementos de la PQ, y asumiendo que sinElUltimo es lineal.
deleteMinPQ (PQ xs) = PQ (sinElMinimo xs)

-- O(b(m(n))) donde b es el costo de borrar (lineal), m es el costo de minimum (lineal) y n la cantidad
--       de elementos de la lista.
sinElMinimo :: Ord a => [a] -> [a]
--PRECOND: la lista no es vacía
sinElMinimo xs = borrar (minimum xs) xs

-- O(n) siendo n la cantidad de elementos de la lista, sobre la cual se hace RE, y
--    asumiendo que la comparación es operación de costo constante.
borrar x []     = []
borrar x (y:ys) = if x==y then ys else y:borrar x ys 


