-- 4.1
import Stack

-- (Stack, emptyS, isEmptyS, push, top, pop, lenS)

-- O(n) donde n es el tamaño de la lista y sobre la cual se hace recursión estructural
-- Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar :: [a] -> Stack a
apilar []     = emptyS
apilar (x:xs) = push x (apilar xs)

{-

[1,2,3,4,5]

1
2
3
4
5    
-}

-- O(n) donde n es el tamaño de la stack 
-- Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar :: Stack a -> [a]
desapilar s = 
    if isEmptyS s
        then []
        else top s : desapilar (pop s)

-- O(n+t(n)) donde n es la posicion a ingresar elemento, t es el costo de top (constante)
-- Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
-- posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
insertarEnPos :: Int -> a -> Stack a -> Stack a
-- PRECOND: la posición dada es válida en la stack
insertarEnPos 0 x s = push x s
insertarEnPos n x s = push (top s) 
                           (insertarEnPos (n-1) x (pop s)) 


--insertarEnPos 2 8 (apilar [1,2,3,4,5]) -> [1,2,8,3,4,5]
