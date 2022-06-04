
data RAList a = MkR Int (Map Int a) (Heap a)

{-
INV.REP: en MkR n m h
        * m y h tienen los mismos elementos.
        * la clave máxima en m es 1 menor que n.
        * n es igual a la cantidad de elementos de m.
-}

-- ==================================================
-- a)

emptyRAL :: RAList a 
emptyRAL = MkR 0 emptyM emptyH
-- Costo: O(1)

-- ==================================================
-- b)

isEmptyRAL :: RAList a -> Bool
isEmptyRAL (MkR n _ _) = n == 0
-- Costo: O(1)

-- ==================================================
-- c)

lengthRAL :: RAList a -> Int
lengthRAL (MkR n _ _) = n
-- Costo: O(1)

-- ==================================================
-- d)

get :: Int -> RAList a -> a
-- PRECOND: el indice dado existe en la lista
get i (MkR _ m _) =
    case (lookupM i m) of
        (Just x) -> x
        Nothing  -> error "No se cumple la precondicion"
-- Costo: O(log N) asumiendo que lookupM tiene costo log N.

-- ==================================================
-- e)

minRAL :: Ord a => RAList a -> a
-- PRECOND: la lista no está vacía.
minRAL (MkR _ _ h) = findMin h
-- Costo: O(1) asumiendo que findMin tiene costo constante.

-- ==================================================
-- f)

add :: Ord a => a -> RAList a -> RAList a
add x (MkR n m h) = 
    MkR (n+1) (assocM n x m) (insertH x h)
-- Costo: O(log N) asumiendo que assocM e insertH tienen costo logN.

-- ==================================================
-- g)

elems :: Ord a => RAList a -> [a]
elems (MkR n m h) = recolectar 0 n m
-- Costo: O(N log N) asumiendo que recolertar tiene costo N log N.

recolectar :: Ord a => Int -> Int -> Map Int a -> [a]
recolertar i n m =
    if (i == n)
        then []
        else case (lookupM i m) of
                (Just x) -> x : recolectar (i+1) n m
                Nothing  -> error "No se cumple la precond"
-- Costo: O(N log N) en cada instancia de la recursión sobre el tamaño del map se usa lookupM de costo log N.

-- ==================================================
-- h)

remove :: Ord a => RAList a -> RAList a
-- PRECOND: la lista no está vacía.
remove (MkR n m h) = 
    case (lookupM (n-1) m) of
        (Just x) -> MkR (n-1) (deleteM (n-1) m) (deleteH x h)
        Nothing  -> error "No se cumple la precondicion"
-- Costo: O(N log N) asumiendo que deleteH tiene costo N log N.

deleteH :: Ord a => a -> Heap a -> Heap a
-- PRECOND: la lista no está vacía.
deleteH x h =
    if x == (findMin h)
        then deleteMin h
        else insertH (findMin h) (deleteH x (deleteMin h))
-- Costo: O(N log N) en cada instancia de la recursión sobre el tamaño de la heap, se utiliza 
-- deleteMin e insertH, de costo log N.

-- ==================================================
-- i)

set :: Ord a => Int -> a -> RAList a -> RAList a
-- PRECOND: el índice dado existe en la lista.
set i x (MkR n m h) =
    case (lookupM i m) of
        (Just y) -> MkR n (assocM i x m) (insertH x (deleteH y h))
        Nothing  -> error "No se cumple la precondicion"
-- Costo: O(N log N) asumiendo que deleteH tiene costo N log N, y lookupM, assocM e insertH
-- tienen costo log N, absorbidos por el primer costo mencionado.

-- ==================================================
-- j)

addAt :: Ord a => Int -> a -> RAList a -> RAList a
-- PRECOND: el índice está entre 0 y la longitud de la lista.
addAt i x (MkR n m h) = 
    MkR (n+1) (desplazar i x n m) (insertH x h)
-- Costo: O(N log N) asumiendo que la operacion desplazar tiene costo N log N.

desplazar :: Ord a => Int -> a -> Int -> Map Int a -> Map Int a
-- PRECOND: el índice i está entre 0 y n.
desplazar i x n m =
    if (i == n)
        then assocM i x m
        else case (lookupM (n-1) m) of
                (Just y) -> assocM n y (desplazar i x (n-1) m)
                Nothing  -> error "No se cumple la precondicion"
-- Costo: O(N log N) se hace recursión sobre el tamaño del map en peor caso (cuando i=0) y en cada
-- instancia de la recursión se usa lookupM y assocM, de costo log N.





 



