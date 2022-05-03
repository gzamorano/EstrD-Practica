-- 2.3
module Set2 (
    Set, emptyS, addS, belongS,
    sizeS, removeS, unionS, setToList
)

where


data Set a = SS [a]  deriving Show
          

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

-- O(n+m) asumiendo que length es lineal, n es la cantidad de elementos de la lista
-- y m también
sizeS (SS xs) = length (sinRep xs)

-- O(n) asumiendo que elem es lineal
sinRep :: Eq a => [a] -> [a]
sinRep []     = []
sinRep (x:xs) = 
  if elem x xs
    then sinRep xs
    else x : sinRep xs


-- O(n) n es la cantidad de elementos de la lista (pueden haber repetidos) que conforma el conjunto
removeS x (SS xs) = 
  if elem x xs
    then SS (sacarTodos x xs)
    else SS xs

-- O(n) ver costo
sacarTodos :: Eq a => a -> [a] -> [a]
sacarTodos x []     = []
sacarTodos x (y:ys) = 
    if x == y
      then sacarTodos x ys
      else y : sacarTodos x ys

-- O(N) revisar el costo
unionS (SS xs) (SS ys) = SS (sinRep (xs++ys))


-- O(n) revisar el costo
setToList (SS xs) = sinRep xs
