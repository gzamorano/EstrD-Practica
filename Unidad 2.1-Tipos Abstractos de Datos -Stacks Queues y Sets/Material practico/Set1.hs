-- 2.1
module Set1 (
    Set, emptyS, addS, belongS,
    sizeS, removeS, unionS, setToList
)

where


type Cant = Int
data Set a = S [a] Cant  deriving Show
          --       length de lista [a]

{-
    son restricciones que no vienen con lenguaje, algo q está atado a la estructura creada.
    INV. REP.: en (S xs n)
        * en xs no existen elementos repetidos
        * n representa la cantidad de elementos de xs 
-}


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
-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList :: Eq a => Set a -> [a]



-- O(1)
emptyS = S [] 0

-- O(n) asumiendo que elem es lineal sobre la cant de elementos de la lista.
addS x (S xs n) =  
    if elem x xs
      then S xs n  
      else S (x:xs) (n+1)

-- agregar x []     = [..]
-- agregar x (y:ys) = 
--   if x == y
--     then y:ys  
--     else y:agregar x ys


-- O(n) asumiendo que elem es lineal sobre la cantidad de elementos de la lista
belongS x (S xs _) = elem x xs

-- O(1) siendo n la cantidad de elementos de la lista, solo lo devuelve
sizeS (S _ n) = n 

-- O(n+m) donde m es el tamaño de la lista y n también
removeS x (S xs n) = 
    if elem x xs
      then S (sacar x xs) (n-1)
      else (S xs n) 

-- O(n)
sacar x (y:ys) = 
    if x == y
      then ys
      else y : sacar x ys

-- O(n^2)  
unionS (S xs _) (S ys _) = 
  let zs = union xs ys
    in S zs (length zs)
  

-- O(n) asumiendo que agregar es lineal
union :: Eq a => [a] -> [a] -> [a]
union []     ys = ys
union (x:xs) ys = agregar x ys

-- O(n)
agregar x []     = [x]
agregar x (y:ys) =
  if x==y
    then y:ys
    else y:agregar x ys



-- O(1) cualquier sea la cantidad de elementos de la lista, la devuelve sin operar sobre ella
setToList (S xs _) = xs

