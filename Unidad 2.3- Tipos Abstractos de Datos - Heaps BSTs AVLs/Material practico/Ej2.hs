data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

-- Las siguientes funciones reciben un árbol binario que cumple las invariantes de BST
-- y sin elementos repetidos.

{-
INV. de BST : en NodeT x ti td
* todos los elementos de ti son menores que x
* todos los elementos de td son mayores que x
* ti y td también cumplen el invariante de BST
-}

belongsBST :: Ord a => a -> Tree a -> Bool
belongsBST = undefined