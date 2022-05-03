-- 5 Implementación de Queue con dos listas fs bs
import Data.S

module Queue3
 (Queue, emptyQ, isEmptyQ, queue, firstQ, dequeue)
 where



data Queue a = Q [a] [a] deriving (Show)

{-
    INV.REP.: en Q fs bs
    * fs jamás se encuentra vacía, salvo que bs esté vacía.
-}


-- operaciones

-- Crea una cola vacía.
emptyQ :: Queue a

-- Dada una cola indica si la cola está vacía.
isEmptyQ :: Queue a -> Bool

-- Dados un elemento y una cola, agrega ese elemento a la cola.
queue :: a -> Queue a -> Queue a

-- Dada una cola devuelve el primer elemento de la cola. 
firstQ :: Queue a -> a

-- Dada una cola la devuelve sin su primer elemento.
dequeue :: Queue a -> Queue a

-- Quitaremos elementos por fs y agregamos por bs

-- O(1)
emptyQ = Q [] []

-- O(1) 
isEmptyQ (Q fs _) = null fs 

-- O(1) asumiendo que null y cons son constantes 
queue x (Q fs bs) = 
    if null fs
        then Q (x:fs) bs
        else Q fs (x:bs)

-- O(1) asumiendo que head es constante
-- parcial
firstQ (Q fs _) = head fs


-- O() constante en la mayoria de los casos. lineal en el peor caso 
-- PEOR CASO: cuando se vacía fs -que pasa cada mucho tiempo- se trasladan los elementos de bs a fs.
-- parcial
dequeue (Q fs bs) = 
    if null (tail fs)
        then Q (reverse bs) [] 
        else Q (tail fs) bs




