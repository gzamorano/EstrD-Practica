-- Recalcular costos de funciones como usuarios de Map,
-- con siguiente interfaz y costos para el TAD


-- emptyM :: Map k v                               --Costo: O(1).
-- assocM :: Ord k => k -> v -> Map k v -> Map k v --Costo: O(log n).
-- lookupM :: Ord k => k -> Map k v -> Maybe v     --Costo: O(log n).
-- deleteM :: Ord k => k -> Map k v -> Map k v     --Costo: O(log n).
-- keys :: Map k v -> [k]                          --Costo: O(n).



-- Costo: O(n) siendo n el tamaño del map, asumiendo que keys es de costo lineal.
-- 1. Propósito: obtiene los valores asociados a cada clave del map.
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m = valores (keys m) m

-- Costo: O(n log n) siendo n el tamaño del map, y asumiendo que lookupM es de costo log n.
-- Por cada elemento del map aparece la op. lookupM 
valores :: Eq k => [k] -> Map k v -> [Maybe v]
valores []     m = []
valores (k:ks) m = lookupM k m : valores ks m

-----------------------------------------------------------------
-----------------------------------------------------------------
                        

-- Costo: O(n log n) siendo n el tamaño de la lista de clave-valor, asumiendo que ahora assocM es de costo log n.
-- Por cada elemento de la lista está involucrada una op. de costo log n (assocM) 
-- 3. Propósito: convierte una lista de pares clave valor en un map.
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap []          = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

