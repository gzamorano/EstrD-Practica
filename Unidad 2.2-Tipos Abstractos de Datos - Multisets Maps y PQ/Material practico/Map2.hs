-- Ejercicio 4.2 map como una lista de pares clave-valor con claves repetidas
module Map2 
 (Map, emptyM, assocM, lookupM, deleteM, keys)
where 

data Map k v = M [(k,v)] 

-- Propósito: devuelve un map vacío
emptyM :: Map k v
-- Propósito: agrega una asociación clave-valor al map.
assocM :: Eq k => k -> v -> Map k v -> Map k v
-- Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v
-- Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
-- Propósito: devuelve las claves del map.
keys :: Eq k => Map k v -> [k]

-- O(1) devuelve map vacío, no hay otra operación
emptyM = M []

-- O(1) asumiendo que la operación cons es constante
assocM k v (M kvs) = M ((k,v):kvs)        

-- O(n) siendo n el tamaño del map, y asumiendo que buscar es de costo lineal.
-- va a devolver el valor de la primer key que encuentre
lookupM k (M kvs) = buscar k kvs
                     

-- O(N) siendo N la cantidad de elementos de la lista sobre la que se realiza RE, asumiendo que
-- la comparación es de costo constante.
buscar :: Eq k => k -> [(k,v)] -> Maybe v
buscar k []       = Nothing
buscar k (kv:kvs) = if k == fst kv 
                        then Just (snd kv)
                        else buscar k kvs

-- O(n) siendo n el tamaño del map, y asumiendo que borrarTodas es de costo lineal.
deleteM k (M kvs) = M (borrarTodas k kvs)

-- O(N) siendo N el tamaño de la lista, sobre la que se hace recursión estructural, y asumiendo
-- que la comparación es op. constante.
borrarTodas :: Eq k => k -> [(k,v)] -> [(k,v)]
borrarTodas k []       = []
borrarTodas k (kv:kvs) = if k == fst kv 
                            then borrarTodas k kvs
                            else kv:borrarTodas k kvs


-- O(n) siendo n el tamaño del map, asumiendo que la función claves es de costo lineal
keys (M kvs) = claves kvs               

-- O(N) siendo N la cantidad de elementos de la lista sobre la que se hace RE,
-- asumiendo que la operación cons en constante.
claves :: Eq k => [(k,v)] -> [k]
claves []       = []
claves (kv:kvs) = fst kv : claves kvs

                  