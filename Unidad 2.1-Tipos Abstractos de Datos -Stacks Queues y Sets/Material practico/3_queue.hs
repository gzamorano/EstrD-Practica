-- 3.3 Implementar funciones sobre Queue como usuario

-- import Queue1
import Queue2

q = queue 6 $ queue 43 $ queue 1 $ queue 0 emptyQ
q1 = queue 7 $ queue 44 $ queue 2 $ queue 1 emptyQ

-- O(n) tiene costo lineal porque a cada elemento hay que sacarlo de la cola y eso tiene costo constante
-- dequeue es constante
-- Cuenta la cantidad de elementos de la cola.
lengthQ :: Queue a -> Int
lengthQ q = 
    if isEmptyQ q
        then 0 
        else 1 + lengthQ (dequeue q)

-- O(e(n)) donde e es el costo de isEmptyQ y n el tamaño de la queue
-- Dada una cola devuelve la lista con los mismos elementos, donde el orden de la lista es el de la cola.
-- Nota: chequear que los elementos queden en el orden correcto.
queueToList :: Queue a -> [a]
queueToList q = 
    if isEmptyQ q 
        then []
        else firstQ q : queueToList (dequeue q)

-- O(n*m) donde n y m son los tamaños de las queues
-- Inserta todos los elementos de la segunda cola en la primera.
unionQ :: Queue a -> Queue a -> Queue a
unionQ q q1 = listToQueue ((queueToList q) ++ (queueToList q1))

-- O(n*m) donde n es el tamaño de la lista sobre la cual se realiza recursión estructural.
--        donde m es el tamñao de la queue  
listToQueue :: [a] -> Queue a
listToQueue xs = listToQueue' (reverse xs)
    where listToQueue' []     = emptyQ
          listToQueue' (x:xs) = queue x (listToQueue' xs) 

















