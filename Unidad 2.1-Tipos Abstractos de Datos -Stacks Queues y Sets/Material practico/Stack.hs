-- 4.2
module Stack
 (Stack, emptyS, isEmptyS, push, top, pop, lenS)
where

data Stack a = S [a] deriving Show

-- Crea una pila vacía.
emptyS :: Stack a

-- Dada una pila indica si está vacía.
isEmptyS :: Stack a -> Bool

-- Dados un elemento y una pila, agrega el elemento a la pila.
push :: a -> Stack a -> Stack a

-- Dada un pila devuelve el elemento del tope de la pila.
top :: Stack a -> a

-- Dada una pila devuelve la pila sin el primer elemento.
pop :: Stack a -> Stack a

-- Dada una pila devuelve la cantidad de elementos que existen en la misma
lenS :: Stack a -> Int

-- O(1)
emptyS = S []

-- O(1) asumiendo que null es constante
isEmptyS (S xs) = null xs

-- O(1) asumiendo que cons es constante
push x (S xs) = S (x:xs)

-- O(1) asumiendo que head es constante
top (S xs) = head xs

-- O(1) asumiendo que tail es constante
pop (S xs) = S (tail xs)

-- O(n) donde n es el tamaño de la stack y asumiendo que length es lineal
lenS (S xs) = length xs



