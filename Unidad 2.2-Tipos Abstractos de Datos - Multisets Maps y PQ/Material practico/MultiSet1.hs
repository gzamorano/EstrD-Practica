-- Ejercicio 6.1 Implementación de MultiSet
module MultiSet1
 (MultiSet, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList)
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

-- O(n^2) donde en es el tamaño del multiset y asumiendo que agregar es cuadrática.
addMS x (MS m) = MS (agregar x m)

-- O(n^2) donde n es el tamaño del map, asumiendo que listToMap y mapToList son operaciones cuadráticas.
agregar :: Eq a => a -> Map a Int -> Map a Int
agregar x m = if x `elem` (keys m)
                then listToMap (aumentar x (mapToList m))
                else assocM x 1 m


-- O(n) donde n es el tamaño del MultiSet, asumiendo que la op. lookupM es lineal.
ocurrencesMS x (MS m)= case (lookupM x m) of
                        (Just x) -> x
                        Nothing  -> 0

-- Si encuentro un elemento en comun entre MS's entonces hay que sumar las ocurrencias.
-- los que no tienen en común, solo se agregan al res. final sin sumar nada.
-- O(n^2) donde n es el tamaño de los multisets y asumiendo que unionM es de costo cuadrático. 
unionMS (MS m1) (MS m2) = MS (unionM m1 m2)  

-- O(n^2) donde n es el tamaño del map, y asumiendo que listToMap y mapToList son de costo
-- cuadrático. 
unionM :: Eq a => Map a Int -> Map a Int -> Map a Int
unionM m1 m2 = listToMap (union (mapToList m1) (mapToList m2))

--O(N^2) donde N es el tamaño de la lista de pares, sobre la que se hace RE, y asumiendo que
-- (pertenece) es de costo lineal.
union :: Eq k => [ (k,Int) ] -> [ (k,Int) ] -> [ (k,Int) ]
union []           kns' = kns'
union ((k,n):kns)  kns' = 
      if k `pertenece` kns'
            then agregarOcurrencias (k,n) (union kns kns')
            else (k,n) : union kns kns' 

-- O(N) donde N es el tamaño de la lista de pares, sobre la que se hace RE.
pertenece :: Eq k => k -> [(k,Int)] -> Bool
pertenece k []           = False
pertenece k ((k',n):kns) = k==k' || pertenece k kns 

-- O(N) donde N es el tamaño de la lista de pares, sobre la que se hace RE.
agregarOcurrencias :: Eq k => (k,Int) -> [(k,Int)] -> [(k,Int)]
agregarOcurrencias (k,n) []            = [(k,n)]
agregarOcurrencias (k,n) ((k',n'):kns) = 
      if k==k'
            then (k',n+n') : kns
            else (k',n') : agregarOcurrencias (k,n) kns



-- Si encuentro un elemento en comun entre MS's entonces hay que sumar las ocurrencias.
-- los que no tienen en común, NO se agregan al resultado final.
-- O()
intersectionMS (MS m1) (MS m2) = undefined
      --MS (interseccionM m1 m2)

--interseccionM :: Eq k => Map k Int -> Map k Int -> Map k Int
-- m1 m2 = listToMap( interseccion (mapToList m1) (mapToList m2) )

--REVISAR
interseccion :: Eq k => [(k,Int)] -> [(k,Int)] -> [(k,Int)]
interseccion []          _    = [] 
interseccion _           []   = []
interseccion ((k,n):kns) kns' =
      if k `pertenece` kns'
            then agregarOcurrencias (k,n) (interseccion kns kns')
            else interseccion kns kns'



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
                        


-- O(N) siendo N la cantidad de elementos de la segunda lista, sobre la que se hace RE,
--     asumiendo que la comparación es operación costo constante.
aumentar :: Eq k => k -> [ (k,Int) ] -> [ (k,Int) ]
aumentar k []       = []
aumentar k (kn:kns) = 
      if k==(fst kn)
        then (fst kn, 1+snd kn) : kns
        else kn : aumentar k kns


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




-- Ejercicio 6.2 Reimplementación de ocurrencias, el resultado será un multiconjunto de
-- caracteres.

