-- 3.2 Implementación de Queue que agrega por delante y quita por el final.
module Queue2
 (Queue, emptyQ, isEmptyQ, queue, firstQ, dequeue)
 where

data Queue a = QQ [a] deriving (Show)

-- operaciones

-- Crea una cola vacía.
emptyQ :: Queue a

-- Dada una cola indica si la cola está vacía.
isEmptyQ :: Queue a -> Bool

-- Dados un elemento y una cola, agrega ese elemento a la cola.
queue :: a -> Queue a -> Queue a

-- Dada una cola devuelve el primer elemento de la cola. -- PARCIAL
firstQ :: Queue a -> a

-- Dada una cola la devuelve sin su primer elemento.
dequeue :: Queue a -> Queue a

-- O(1)
emptyQ = QQ  []

-- O(1) asumiendo que null es constante
isEmptyQ (QQ xs) = null xs

-- O(1) asumiendo que cons es constante
queue x (QQ xs) = QQ (x:xs)

-- O(n) donde n es el tamaño de la queue y asumiendo que last es lineal
firstQ (QQ xs) = last xs

-- O(n) donde n es el tamaño de la queue y asumiendo que sinElUltimo es lineal
dequeue (QQ xs) = QQ (sinElUltimo xs)

-- O(n) donde n es el tamaño de la lista y sobre la cual se hace recursión estructural,
-- y asumiendo que null es constante
sinElUltimo :: [a] -> [a]
sinElUltimo []     = [] 
sinElUltimo (x:xs) = 
    if null xs
        then []
        else x : sinElUltimo xs



