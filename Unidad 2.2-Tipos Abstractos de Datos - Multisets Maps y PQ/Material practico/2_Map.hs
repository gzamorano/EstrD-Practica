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
--agruparEq :: Eq k => [(k, v)] -> Map k [v]
-- [(1,5),(2,6),(3,7),(2,9)] -> [ (1,[5]),(2,[6,9]),(3,[7]),(2,[9]) ]
--agruparEq kvs = listToMap (agruparIguales kvs)


--agruparIguales :: Eq k => [(k, v)] ->  [(k, v)]
--agruparIguales = undefined


---


-- O()
-- 5 Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
--             cada número asociado con dichas claves. -- REVISAR
incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar ks m = listToMap (aumentarKeys ks (mapToList m))

aumentarKeys :: Eq k => [k] -> [(k,Int)] -> [(k,Int)]
-- [1,3] [(1,5),(2,6),(3,7)] -> [(1,6),(2,6),(3,8)]
aumentarKeys []     kns = kns
aumentarKeys (k:ks) kns =  juntarKns (aumentarK k kns) (aumentarKeys ks kns)


aumentarK :: Eq k => k -> [(k,Int)] -> [(k,Int)]
aumentarK k []       = []
aumentarK k (kn:kns) = 
      if k==(fst kn)
        then (fst kn, 1+(snd kn)) : aumentarK k kns
        else kn : aumentarK k kns


juntarKns :: Eq k => [(k,Int)] -> [(k,Int)] -> [(k,Int)] 
juntarKns []  kns' = kns'
juntarKns kns []   = kns
juntarKns (kn:kns) (kn':kns') = 
      if (fst kn) == (fst kn')
        then (fst kn',max (snd kn) (snd kn')) : juntarKns kns kns'
        else [kn,kn'] ++ juntarKns kns kns'



-- O()
-- 6 Propósito: dado dos maps se agregan las claves y valores del primer map en el seg
mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
mergeMaps m1 m2 = listToMap (juntarLs (mapToList m1) (mapToList m2))


juntarLs :: Eq k => [(k,v)] -> [(k,v)] -> [(k,v)]
juntarLs []         kvs2 = kvs2
juntarLs (kv1:kvs1) kvs2 = 
      if estaEn (fst kv1) kvs2
            then kv1 : juntarLs kvs1 kvs2
            else juntarLs kvs1 kvs2


estaEn :: Eq k => k -> [(k,v)] -> Bool
estaEn k []       = False
estaEn k (kv:kvs) = k == (fst kv) || estaEn k kvs

-- ... valor q tiene k y agregarlo a m2, usa keys m2