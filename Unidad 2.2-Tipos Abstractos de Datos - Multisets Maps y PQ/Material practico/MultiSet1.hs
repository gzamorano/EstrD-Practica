-- Ejercicio 6.1 Implementación de MultiSet
module MultiSet1
 (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList)
where

import Map1


data MultiSet a = MS (Map a Int) deriving Show

-- Propósito: denota un multiconjunto vacío.
emptyMS :: MultiSet a

-- Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al
-- multiconjunto.
addMS :: Ord a => a -> MultiSet a -> MultiSet a

-- Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
-- elemento en el multiconjunto.
ocurrencesMS :: Ord a => a -> MultiSet a -> Int

-- Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
-- ambos multiconjuntos.
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a 

-- Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
-- multiconjuntos tienen en común.
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a 

-- Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
-- su cantidad de ocurrencias.
multiSetToList :: Ord a => MultiSet a -> [(a, Int)]

-- O(1) asumiendo que emptyM es de costo constante.
emptyMS = MS emptyM

-- ==================================================================

-- O(n) donde en es el tamaño del multiset y asumiendo que agregar es de costo lineal.
addMS x (MS m) = MS (agregar x m)

-- O(n) donde n es el tamaño del map, asumiendo que cantKey es de costo lineal.
agregar :: Eq a => a -> Map a Int -> Map a Int
agregar x m = assocM x ((cantKey x m)+1) m

-- ==================================================================

-- O(n) donde n es el tamaño del MultiSet, asumiendo que la op. cantKey es de costo lineal.
ocurrencesMS x (MS m)= cantKey x m

-- ==================================================================

-- O(n^2) donde n es el tamaño de los multisets y asumiendo que mapToList es de costo cuadrático.
unionMS (MS m1) (MS m2) = MS (unionM (mapToList m1) m2)  

-- O(n) siendo n el tamaño de la lista clave-valor, y asumiendo que agregarOcurrencias es
-- de costo lineal.
unionM :: Eq k => [(k,Int)] -> Map k Int -> Map k Int
unionM []          m = m
unionM ((k,n):kvs) m = agregarOcurrencias (k,n) (unionM kvs m)

-- O(n) siendo n el tamaño del map, y asumiendo que assocM y cantKey son de orden lineal.
agregarOcurrencias :: Eq k => (k,Int) -> Map k Int -> Map k Int
agregarOcurrencias (k,n) m = assocM k ((cantKey k m)+n) m


-- ==================================================================

-- Si encuentro un elemento en comun entre MS's entonces hay que sumar las ocurrencias.
-- los que no tienen en común, NO se agregan al resultado final. REVISAR
-- O()
intersectionMS (MS m1) (MS m2) = MS (interseccionM (mapToList m1) m2)

-- O(n) siendo n el tamaño de la lista clave-valor, y asumiendo que agregarOcurrencias es
-- de costo lineal.
interseccionM :: Eq k => [(k,Int)] -> Map k Int -> Map k Int
interseccionM []          m = []
interseccionM ((k,n):kvs) m = 
      if k `elem` (keys m)
            then agregarOcurrencias (k,n) (interseccionM kvs m)
            else interseccionM kvs m
      


-- ==================================================================

-- O(n^2) siendo n el tamaño del multiset y asumiendo que mapToList es de costo cuadrático. 
multiSetToList (MS m) = mapToList m 



ms1 = addMS 1 
    $ addMS 5
    $ addMS 7 
    $ addMS 1 emptyMS

ms2 = addMS 3
    $ addMS 9
    $ addMS 1
    $ addMS 7
    $ addMS 10 emptyMS






---------------------------------------------
-- AUXILIARES


-- O(n^2) siendo n la cantidad de elementos del map, y asumiendo que 'valores' es cuadrática.
-- 1. Propósito: obtiene los valores asociados a cada clave del map.
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m = valores (keys m) m

-- O(N^2) siendo N la cantidad de keys de la lista, sobre la que se hace RE,
--        y asumiendo que lookupM es lineal.
valores :: Eq k => [k] -> Map k v -> [Maybe v]
valores []     m = []
valores (k:ks) m = lookupM k m : valores ks m


-- O(n) siendo n el tamaño del map, y asumiendo que lookupM es de costo lineal 
cantKey :: Eq k => k -> Map k Int -> Int
cantKey k m = case lookupM k m of
                  (Just n) -> n
                  Nothing  -> 0 



-- O(N^2) siendo N la cantidad de pares de la lista, sobre la que se hace RE,
--        y asumiendo que assocM es lineal.
-- 3. Propósito: convierte una lista de pares clave valor en un map.
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap []          = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

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


-- O(n^2) siendo n la cantidad de elementos del primer map y asumiendo que unir es de orden 
--        cuadrático.
mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
mergeMaps m1 m2 = unir (mapToList m1) m2

-- O(N^2) siendo N la cantidad de elementos de la lista y asumiendo que assocM es lineal
unir :: Eq k => [ (k,v) ] -> Map k v -> Map k v
unir []       m = m 
unir (kv:kvs) m = assocM (fst kv) (snd kv) (unir kvs m) 




