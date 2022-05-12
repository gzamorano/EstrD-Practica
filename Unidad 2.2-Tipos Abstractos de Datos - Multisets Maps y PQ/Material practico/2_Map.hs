import Map1
-- Ejercicio 3

--  (Map, emptyM, assocM, lookupM, deleteM, keys)

unMap = assocM "a" 5
      $ assocM "b" 6
      $ assocM "c" 7
      $ assocM "d" 8 
      $ emptyM

unMap2 = assocM "e" 10
      $ assocM "f" 11
      $ assocM "g" 12
      $ assocM "a" 8 
      $ assocM "b" 23
      $ emptyM

-- O(n^2) siendo n la cantidad de elementos del map, y asumiendo que 'valores' es cuadrática.
-- 1. Propósito: obtiene los valores asociados a cada clave del map.
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m = valores (keys m) m

-- O(N^2) siendo N la cantidad de keys de la lista, sobre la que se hace RE,
--        y asumiendo que lookupM es lineal.
valores :: Eq k => [k] -> Map k v -> [Maybe v]
valores []     m = []
valores (k:ks) m = lookupM k m : valores ks m
                        
-----------------------------------------------------------------
-----------------------------------------------------------------

-- O(N^2) siendo N la cantidad de keys de la lista, y asumiendo que
--       todosPertenecen es cuadrática.
-- 2. Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas ks m = todosPertenecen ks (keys m)

-- O(N^2) siendo N la cantidad de keys de la lista, sobre la que se hace RE, 
--       y asumiendo que elem es lineal.
todosPertenecen :: Eq k => [k] -> [k] -> Bool
todosPertenecen []     ks' = False
todosPertenecen (k:[]) ks' = k `elem` ks'
todosPertenecen (k:ks) ks' = k `elem` ks' && todosPertenecen ks ks' 

-----------------------------------------------------------------
-----------------------------------------------------------------

-- O(N^2) siendo N la cantidad de pares de la lista, sobre la que se hace RE,
--        y asumiendo que assocM es lineal.
-- 3. Propósito: convierte una lista de pares clave valor en un map.
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap []          = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

-----------------------------------------------------------------
-----------------------------------------------------------------

-- O(n^2) siendo n la cantidad de elementos del map y asumiendo que valuesM es de costo cuadrático.
-- 4. Propósito: convierte un map en una lista de pares clave valor.
mapToList :: Eq k => Map k v -> [(k, v)]
mapToList m = ksvsToList (keys m) (valuesM m)

-- O(n+m) donde n es la cantidad de keys de la primer lista y m la cantidad de valores
--        de la segunda, sobre las cuales se hace RE.  
ksvsToList :: [k] -> [Maybe v] -> [(k, v)]
ksvsToList []     _      = []
ksvsToList (k:ks) (v:vs) =  
      (k,case v of (Just x) -> x) : ksvsToList ks vs

-----------------------------------------------------------------
-----------------------------------------------------------------

-- O(l(N^2)) siendo N la cantidad de elementos de la lista, l el costo de listToMap,
--          y asumiendo que agruparIguales es de costo cuadrático.
-- 5. Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
--              la misma clave
agruparEq :: Eq k => [ (k,v) ] -> Map k [v]
agruparEq kvs = listToMap (agruparIguales kvs)

-- O(N^2) siendo N la cantidad de elementos de la lista, sobre la que se hace RE,
--        y asumiendo que 'agrupar' es lineal.
agruparIguales :: Eq k => [ (k,v) ] ->  [ (k,[v]) ]
agruparIguales []       = []
agruparIguales (kv:kvs) = agrupar kv (agruparIguales kvs)

-- O(N) siendo N la cantidad de elementos de la segunda lista, sobre la que se realizar RE,
--      y asumiendo que la comparación es una operación constante.
agrupar :: Eq k => (k,v) -> [ (k,[v]) ] -> [ (k,[v]) ]
agrupar (k,v) []         = [(k,[v])]
agrupar (k,v) (kvs:kvss) = 
      if k == fst kvs
            then (k,v:snd kvs) : kvss
            else kvs : agrupar (k,v) kvss

-----------------------------------------------------------------
-----------------------------------------------------------------

-- O(l*a*m(n^2)) siendo l el costo de listToMap, a el costo de aumentarKeys, m costo de mapToList,
--        y n la cantidad de elementos del map.
-- 6. Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
--             cada número asociado con dichas claves.
incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar ks m = listToMap (aumentarKeys ks (mapToList m))

-- O(N^2) siendo N la cantidad de elementos de la lista de keys, sobre la que se hace RE,
--       y asumiendo que aumentarK es lineal.
aumentarKeys :: Eq k => [k] -> [ (k,Int) ] -> [ (k,Int) ]
aumentarKeys []     kns = kns
aumentarKeys (k:ks) kns = aumentarK k (aumentarKeys ks kns)

-- O(N) siendo N la cantidad de elementos de la segunda lista, sobre la que se hace RE,
--     asumiendo que la comparación es operación costo constante.
aumentarK :: Eq k => k -> [ (k,Int) ] -> [ (k,Int) ]
aumentarK k []       = []
aumentarK k (kn:kns) = 
      if k==(fst kn)
        then (fst kn, 1+snd kn) : kns
        else kn : aumentarK k kns

-----------------------------------------------------------------
-----------------------------------------------------------------


-- O(n^2) siendo n la cantidad de elementos del primer map y asumiendo que unir es de orden 
--        cuadrático.
-- 7. Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
--              una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
mergeMaps m1 m2 = unir (mapToList m1) m2

-- O(N^2) siendo N la cantidad de elementos de la lista y asumiendo que assocM es lineal
unir :: Eq k => [ (k,v) ] -> Map k v -> Map k v
unir []       m = m 
unir (kv:kvs) m = assocM (fst kv) (snd kv) (unir kvs m) 


-----------------------------------------------------------------
-----------------------------------------------------------------
-- Ejercicio 5

-- O(N^2) siendo N el tamaño de la lista, y asumiendo que listToMap e indizar son op.
-- de costo cuadrático, que suceden una después de otra.
--Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
--su posición en la lista.
indexar ::  [a] -> Map Int a
indexar xs = listToMap (indizar xs)

-- O(N^2) siendo N el tamaño de la lista, sobre la que se hace RE, y asumiendo que
-- agregar es de costo lineal.
indizar :: [a] -> [(Int,a)]
indizar []     = []
indizar (x:xs) = agregar x (indizar xs)

-- O(N) siendo N el tamaño de la lista de pares
agregar :: a -> [(Int,a)] -> [(Int,a)]
agregar x []       = [(0,x)]
agregar x (nx:nxs) = (fst nx+1,snd nx) : agregar x nxs

-- ==============================================================

-- O(n^2) asumiendo que listToMap, contar son operaciones de costo cuadrático y se suceden
-- una despúes de otra.
--Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen
--en el string, y los valores la cantidad de veces que aparecen en el mismo.
ocurrencias :: String -> Map Char Int
ocurrencias cs = listToMap (contar cs) 

-- O(N^2) siendo N el tamaño de la lista de caracteres, sobre la que se hace RE, y asumiendo
-- que consolidar es de costo lineal.
contar :: Eq c => [c] -> [(c,Int)]
contar []     = []
contar (c:cs) = consolidar c (contar cs) 

-- O(N) siendo N el tamaño de la lista de pares, sobre la que se hace RE, y asumiendo que
-- la comparación es una op. costo constante.
consolidar :: Eq c => c -> [(c,Int)] -> [(c,Int)]
consolidar c []       = [(c,1)]
consolidar c (cn:cns) = if c==fst cn
                          then (c,1+snd cn) : cns
                          else cn:consolidar c cns
