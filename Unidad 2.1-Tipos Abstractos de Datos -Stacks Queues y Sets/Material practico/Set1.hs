module Set1 (
    Set, emptyS, addS, belongS,
    sizeS, removeS, unionS, setToList
)

where

-- l le puso Amount con type Int
type Cant = Int
data Set a = S [a] Cant deriving Show
          --       length de lista [a]

{-
    son restricciones que no vienen con lenguaje, algo q está atado a la estructura creada.
    INV. REP.: en (S [a] Cant)
        * en [a] no existen elementos repetidos
        * Cant representa la cantidad de elementos de [a]
-}



-- operaciones (ya se dan creadas)

-- O(1)
--Crea un conjunto vacío.
emptyS :: Set a
emptyS = S [] 0

-- O(n) asumiendo que elem es lineal sobre la cant de elementos de la lista.
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS :: Eq a => a -> Set a -> Set a
addS x (S xs n) =  
    if elem x xs
      then S xs n  
      else S (x:xs) (n+1)

-- O(n) asumiendo que elem es lineal sobre la cantidad de elementos de la lista
-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongS :: Eq a => a -> Set a -> Bool
belongS x (S xs _) = elem x xs



-- Devuelve la cantidad de elementos distintos de un conjunto.
sizeS :: Eq a => Set a -> Int
sizeS (S _ n) = n 


-- O(n+m) donde m es el tamaño de la lista y n también
-- Borra un elemento del conjunto. la función es total, no rompe.
removeS :: Eq a => a -> Set a -> Set a
removeS x (S xs n) = 
    if elem x xs
      then S (sacarS x xs) (n-1)
      else (S xs n) 


sacarS x (y:ys) = 
    if x == y
      then ys
      else y : sacarS x ys
    
-- O(n+m) asumiendo que append ++ es lineal y la suma también
-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs n) (S ys m) = S (xs++ys) (n+m)

-- O(1) cualquier sea la cantidad de elementos de la lista, la devuelve sin operar sobre ella
-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto
setToList :: Eq a => Set a -> [a]
setToList (S xs n) = xs









