-- Ejercicio 4.3 map como dos listas, una de claves y otra de valores, donde la clave ubicada
-- en la posición i está asociada al valor en la misma posición, pero de la otra lista.
module Map3
 (Map, emptyM, assocM, lookupM, deleteM, keys)
where 

data Map k v = M [k] [v] deriving Show

{-
   INV.REP.: en M ks vs
   * en la lista ks, la clave ubicada en la posición i está asociada al valor en la misma posición
     de la lista vs.
-}

-- Propósito: devuelve un map vacío
emptyM :: Map k v
-- Propósito: agrega una asociación clave-valor al map.
assocM :: Eq k => k -> v -> Map k v -> Map k v
-- Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v
-- Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
-- Propósito: devuelve las claves del map. (DISTINTAS)
keys :: Eq k => Map k v -> [k]

-- O(1)
emptyM = M [] [] 

-- O(1) asumiendo que la operación cons es constante.
assocM k v (M ks vs) = M (k:ks) (v:vs)

-- O(n) siendo n el tamaño del map, asumiendo que buscar es de orden lineal.           
lookupM k (M ks vs) = buscar k ks vs 

-- O(n) siendo n el tamaño del map, asumiendo que borrarTodos y listToMap son de costo lineal.
deleteM k (M ks vs) = listToMap (borrarTodos k ks vs)

-- O(n^2) siendo n el tamaño del map y asumiendo que sinRep es de costo cuadrático.
keys (M ks _) = sinRep ks            

-- O(N) siendo N el tamaño de la lista de keys, sobre la que se hace RE, asumiendo que
-- la comparación tiene costo constante.
buscar :: Eq k => k -> [k] -> [v] -> Maybe v
buscar k []      _      = Nothing
buscar k (k':ks) (v:vs) = if k==k'
                            then Just v
                            else buscar k ks vs


-- O(N) siendo N el tamaño de la lista de keys, sobre la que se hace RE, asumiendo que
-- la comparación tiene costo constante.
borrarTodos :: Eq k => k -> [k] -> [v] -> [(k,v)]
borrarTodos k []      _      = []
borrarTodos k (k':ks) (v:vs) = if k==k'
                                then borrarTodos k ks vs 
                                else (k',v) : borrarTodos k ks vs

-- O(N) siendo N el tamaño de la lista de pares, sobre la que se realiza RE, asumiendo que
-- assocM es de costo constante.              
listToMap :: Eq k => [(k,v)] -> Map k v
listToMap []          = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

-- O(N^2) siendo N el tamaño de la lista de keys, y asumiendo que elem es lineal
sinRep :: Eq k => [k] -> [k]
sinRep []     = [] 
sinRep (k:ks) =
      if k `elem` ks
            then sinRep ks
            else k : sinRep ks


unMap = assocM "a" 5
      $ assocM "b" 6
      $ assocM "c" 7
      $ assocM "d" 8 
      $ assocM "a" 8 
      $ emptyM

unMap2 = assocM "e" 10
      $ assocM "f" 11
      $ assocM "g" 12
      $ assocM "a" 8 
      $ assocM "b" 23
      $ emptyM