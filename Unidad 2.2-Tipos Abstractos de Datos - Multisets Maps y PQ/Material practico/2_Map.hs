import Map1
-- Ejercicio 3

--  (Map, emptyM, assocM, lookupM, deleteM, keys)

unMap = assocM "a" 5
      $ assocM "b" 6
      $ assocM "c" 7
      $ assocM "d" 8 
      $ emptyM

-- O()
-- 1 Propósito: obtiene los valores asociados a cada clave del map.
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m = valores (keys m) m

valores :: Eq k => [k] -> Map k v -> [Maybe v]
valores []     m = []
valores (k:ks) m = lookupM k m : valores ks m  

-- O()
-- 2 Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
-- todasAsociadas ["b","d","a"] unMap -> True .. revisar el caso base
todasAsociadas ks m = todosPertenecen ks (keys m)

-- O()
todosPertenecen :: Eq k => [k] -> [k] -> Bool
todosPertenecen []     ks' = False
todosPertenecen (k:[]) ks' = k `elem` ks'
todosPertenecen (k:ks) ks' = k `elem` ks' && todosPertenecen ks ks' 

-- O()
-- 3 Propósito: convierte una lista de pares clave valor en un map.
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap []          = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

-- O()
-- 4 Propósito: convierte un map en una lista de pares clave valor.
mapToList :: Eq k => Map k v -> [(k, v)]
mapToList m = ksvsToList (keys m) (valuesM m)

ksvsToList :: [k] -> [Maybe v] -> [(k, v)]
ksvsToList []     _      = []
ksvsToList _      []     = []
ksvsToList (k:ks) (v:vs) = (k,fromJust v) : ksvsToList ks vs

-- O(1)
fromJust :: Maybe a -> a
-- PRECOND: se ingresa un Just
fromJust (Just x) = x

-- 
-- 5 Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
--              la misma clave
agruparEq :: Eq k => [(k, v)] -> Map k [v]
-- [(1,5),(2,6),(3,7),(2,9)] -> [ (1,[5]),(2,[6,9]),(3,[7]),(2,[9]) ]
agruparEq kvs = listToMap (agruparIguales kvs)


agruparIguales :: Eq (k,v) => [(k, v)] ->  [(k, v)]
agruparIguales = undefined


    -- if k `elem` (keys kvs)
    --     then 
    --     else
    -- kv ... agruparEq kvs 