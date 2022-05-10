data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show
{-
INV. de BST : en NodeT x ti td
* todos los elementos de ti son menores que x
* todos los elementos de td son mayores que x
* ti y td también cumplen el invariante de BST
-}

tree1 = NodeT 10 (NodeT 6 
                   (NodeT 3 EmptyT 
                     (NodeT 4 EmptyT EmptyT)) 
                   (NodeT 7 EmptyT EmptyT)) 
                 (NodeT 15 
                     (NodeT 12 EmptyT EmptyT) EmptyT)

{-

                    10
        6                       15       
    3       7               12
      4

-}

-- Las siguientes funciones reciben un árbol binario que cumple las invariantes de BST
-- y sin elementos repetidos.


-- O(log n)
-- Propósito: dado un BST dice si el elemento pertenece o no al árbol.
belongsBST :: Ord a => a -> Tree a -> Bool
belongsBST x EmptyT          = False
belongsBST x (NodeT y ti td) = 
    if x==y
        then True
        else if x<y
            then belongsBST x ti
            else belongsBST x td

-- =====================================================

-- O(log n) siendo n la profundidad del árbol, en peor caso, recorre una rama entera
-- Propósito: dado un BST inserta un elemento en el árbol.
insertBST :: Ord a => a -> Tree a -> Tree a
insertBST x EmptyT          = (NodeT x EmptyT EmptyT)
insertBST x (NodeT y ti td) =
    if x==y
        then (NodeT y ti td)
        else if x<y
            then NodeT y (insertBST x ti) td 
            else NodeT y ti (insertBST x td)

-- =====================================================

-- O(log N)
-- Propósito: dado un BST borra un elemento en el árbol.
deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST x EmptyT = EmptyT
deleteBST x (NodeT y ti td) = 
    if x==y
        then rearmarBST ti td
        else if x<y
            then NodeT y (deleteBST x ti) td
            else NodeT y ti (deleteBST x td)

-- O(log N)
rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
rearmarBST ti EmptyT = ti
rearmarBST ti td     = NodeT (maxBST ti) (borrarMaxBST ti) td

-- O(log N)
maxBST :: Ord a => Tree a -> a
-- PRECOND: el árbol no es vacío
maxBST (NodeT x _ EmptyT) = x
maxBST (NodeT _ _ td)     = maxBST td

-- O(log N)
borrarMaxBST :: Ord a => Tree a -> Tree a 
-- PRECOND: el árbol no es vacío
borrarMaxBST (NodeT _ ti EmptyT) = ti
borrarMaxBST (NodeT x ti td)     = NodeT x ti (borrarMaxBST td)

-- =====================================================
 -- REVISAR ESTE EJER
-- O(log N)
-- Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
-- splitMinBST :: Ord a => Tree a -> (a, Tree a)
-- -- PRECOND: el árbol no es vacío
-- splitMinBST (NodeT x EmptyT td) = (x, td)
-- splitMinBST (NodeT x ti td) = NodeT x (sinElMin ti) td


-- sinElMin :: Ord a => Tree a -> Tree a
-- -- PRECOND: el árbol no es vacío
-- sinElMin (NodeT _ EmptyT EmptyT) =  EmptyT
-- sinElMin (NodeT x ti td) = NodeT x (sinElMin ti) td


-- =====================================================


-- O(N2)
-- Propósito: indica si el árbol cumple con los invariantes de BST.
esBST :: Ord a => Tree a -> Bool
esBST EmptyT          = True
esBST (NodeT x ti td) = esMayorATodos x ti && esMenorATodos x td

esMayorATodos :: Ord a => a -> Tree a -> Bool
esMayorATodos x EmptyT          = True
esMayorATodos x (NodeT y ti td) = x > y && esMayorATodos x ti && esMayorATodos x td


esMenorATodos :: Ord a => a -> Tree a -> Bool
esMenorATodos x EmptyT          = True
esMenorATodos x (NodeT y ti td) = x < y && esMenorATodos x ti && esMenorATodos x td




