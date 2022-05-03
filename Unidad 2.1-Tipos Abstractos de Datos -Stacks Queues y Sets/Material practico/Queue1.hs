-- 3.1 Implementación de Queue que agrega por al final y quita por delante.
module Queue1
 (Queue, emptyQ, isEmptyQ, queue, firstQ, dequeue)
 where

data Queue a = Q [a] deriving (Show)

-- operaciones

-- Crea una cola vacía.
emptyQ :: Queue a

-- Dada una cola indica si la cola está vacía.
isEmptyQ :: Queue a -> Bool

-- Dados un elemento y una cola, agrega ese elemento a la cola.
queue :: a -> Queue a -> Queue a

-- Dada una cola devuelve el primer elemento de la cola.
firstQ :: Queue a -> a  -- PARCIAL: la cola no es vacía

-- Dada una cola la devuelve sin su primer elemento.
dequeue :: Queue a -> Queue a  -- PARCIAL: la cola no es vacía

-- O(1)
emptyQ = Q []

-- O(1) asumiendo que null es constante
isEmptyQ (Q xs) = null xs

-- O(n) asumiendo que append es lineal 
queue x (Q xs) = Q (xs++[x])

-- O(1) asumiendo que head es constante 
firstQ (Q xs) = head xs

-- O(1) asumiendo que tail es constante
dequeue (Q xs) = Q (tail xs)
