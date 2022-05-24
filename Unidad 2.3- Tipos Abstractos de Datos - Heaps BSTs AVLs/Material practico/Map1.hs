-- Implementación ejercicio 4.1
module Map1 
 (Map, emptyM, assocM, lookupM, deleteM, keys)
where 

data Map k v = M [(k,v)] deriving Show

{-
    INV.REP.: para M kvs
    * en kvs no existen claves repetidas
-}

-- Propósito: devuelve un map vacío
emptyM :: Map k v
-- Propósito: agrega una asociación clave-valor al map.
assocM :: Eq k => k -> v -> Map k v -> Map k v
-- Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v
-- Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
-- Propósito: devuelve las claves del map.
keys :: Map k v -> [k]

emptyM             = M []                  -- O(1)
assocM k v (M kvs) = M (asociar k v kvs)   -- O(n)
                  -- M ((k,v) : kvs)       ASÍ NO, porque puede violar el Invariante!!
lookupM k  (M kvs) = buscar k kvs          -- O(n)
deleteM k  (M kvs) = M (borrar k kvs)      -- O(n)
keys       (M kvs) = claves kvs            -- O(n)

-- O(n)
buscar :: Eq k => k -> [(k,v)] -> Maybe v
buscar k []            = Nothing
buscar k ((k',v'):kvs) = if k==k' then Just v' else buscar k kvs

-- O(n)
claves :: [(k,v)] -> [k]
claves []          = []
claves ((k,v):kvs) = k : claves kvs

-- O(n)
asociar :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
asociar k v []            = [(k,v)]
asociar k v ((k',v'):kvs) = if k==k' then (k',v):kvs else (k',v') : asociar k v kvs

-- O(n)
borrar :: Eq k => k -> [(k,v)] -> [(k,v)]
borrar k []            = []
borrar k ((k',v'):kvs) = if k==k' then kvs else (k',v') : borrar k kvs