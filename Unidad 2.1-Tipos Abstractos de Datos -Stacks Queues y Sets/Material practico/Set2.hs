-- 2.3
module Set2 (
    Set, emptyS, addS, belongS,
    sizeS, removeS, unionS, setToList
)
where

data Set a = SS [a] 
          
-- operaciones (ya se dan creadas)

--Crea un conjunto vacío.
emptyS :: Set a
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS :: Eq a => a -> Set a -> Set a
-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongS :: Eq a => a -> Set a -> Bool
-- Devuelve la cantidad de elementos distintos de un conjunto.
sizeS :: Eq a => Set a -> Int
-- Borra un elemento del conjunto. la función es total, no rompe.
removeS :: Eq a => a -> Set a -> Set a
-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
unionS :: Eq a => Set a -> Set a -> Set a
-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto
setToList :: Eq a => Set a -> [a]



-- O(1) devuelve un Set vacío
emptyS = SS []

-- O(1) agrega el elemento a la lista, sin importar su longitud
addS x (SS xs) = SS (x:xs)
    
-- O(N) con N la cantidad de elementos de la lista
belongS x (SS xs) = elem x xs

-- O(N) asumiendo que length y sinREP tienen costo lineal, N es la cantidad de elementos de la lista
sizeS (SS xs) = length (sinRep xs)

-- O(n) siendo n la cantidad de elementos de la lista, sobre la que se hace RE, 
-- y asumiendo que elem es lineal
sinRep :: Eq a => [a] -> [a]
sinRep []     = []
sinRep (x:xs) = 
  if elem x xs
    then sinRep xs
    else x : sinRep xs


-- O(N) N es la cantidad de elementos de la lista (pueden haber repetidos) que conforma el conjunto
removeS x (SS xs) = 
  if elem x xs
    then SS (sacarTodos x xs)
    else SS xs

-- O(n) siendo n la cantidad de elementos de la lista, sobre la cual se hace RE, y asumiendo
-- que la comparación es de costo constante
sacarTodos :: Eq a => a -> [a] -> [a]
sacarTodos x []     = []
sacarTodos x (y:ys) = 
    if x == y
      then sacarTodos x ys
      else y : sacarTodos x ys

-- O(s*a(N+M)) siendo N la cantidad de elementos del primer set, M la cantidad del segundo,
-- 'a' el costo de append (lineal) y s el costo de sinRep (lineal)
unionS (SS xs) (SS ys) = SS (sinRep (xs++ys))


-- O(N) siendo N la cantidad de elementos del set, y asumiendo que sinRep es de costo lineal.
setToList (SS xs) = sinRep xs
