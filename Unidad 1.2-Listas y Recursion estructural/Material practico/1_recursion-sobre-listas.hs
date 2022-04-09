-- 1 
sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatoria (n:ns) = n + sumatoria ns 

-- 2
longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

-- 3
sucesores :: [Int] -> [Int]
sucesores []     = []  
sucesores (n:ns) = (n+1) : sucesores ns

-- 4 
conjuncion :: [Bool] -> Bool
conjuncion []     = False
conjuncion (b:[]) = b
conjuncion (b:bs) = b && conjuncion bs

-- 5 
disyuncion :: [Bool] -> Bool
disyuncion []     = False
disyuncion (b:bs) = b || disyuncion bs

-- 6 
aplanar :: [[a]] -> [a]
aplanar []     = []
aplanar (x:[]) = x
aplanar (x:xs) = x ++ aplanar xs

-- 7
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ []     = False
pertenece a (x:xs) = (x == a) || pertenece a xs

-- 8
apariciones :: Eq a => a -> [a] -> Int
apariciones _ []     = 0
apariciones a (x:xs) = (if x == a then 1 else 0) 
                      + (apariciones a xs)

  

-- 9
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _   []     = []
losMenoresA num (n:ns) = if n<num
                          then n : losMenoresA num ns
                          else losMenoresA num ns


-- 10
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ []     = []
lasDeLongitudMayorA n (x:xs) = if (longitud x > n)
                                then x : lasDeLongitudMayorA n xs
                                else lasDeLongitudMayorA n xs


-- 11
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal []     y = [y]
agregarAlFinal (x:xs) y = x : agregarAlFinal xs y


-- 12 
concatenar :: [a] -> [a] -> [a]
concatenar []     ys = ys
concatenar (x:xs) ys = x : concatenar xs ys


-- 13
reversa :: [a] -> [a]
reversa []     = []
reversa (x:xs) = reversa xs ++ [x]

-- 14

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos []     _         = []
zipMaximos _      []        = [] 
zipMaximos (n:ns) (m:ms)    = maximoEntre n m : zipMaximos ns ms 

-- la anterior función se puede resolver de otra manera, en donde si una lista no tiene elemento a comparar y
-- la otra sí, entonces devuelvo la lista con elementos 
zipMaximos' :: [Int] -> [Int] -> [Int]
zipMaximos' []      ys    = ys
zipMaximos' xs      []    = xs
zipMaximos' (x:xs) (y:ys) = maximoEntre x y : zipMaximos' xs ys 


maximoEntre :: Int -> Int -> Int
maximoEntre n m = if(n>m)
                    then n
                    else m

-- 15 
elMinimo :: Ord a => [a] -> a
elMinimo []     = error "No existe minimo en una lista vacia"
elMinimo (x:[]) = x
elMinimo (x:xs) = elMenorEntre x (elMinimo xs)

elMenorEntre :: Ord a => a -> a -> a
elMenorEntre x y = if (x < y) then x else y
 

