-- 1
factorial :: Int -> Int
factorial 0 = 1
factorial n = if(n < 0) 
                then error "No se puede hacer factorial de numero negativo" 
                else n * factorial (n-1)

-- 2
cuentaRegresiva :: Int -> [Int]
-- PRECOND: el número es mayor o igual que cero
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)

-- 3
repetir :: Int -> a -> [a]
-- PRECOND: el número es mayor o igual que cero
repetir 0 _ = []
repetir n x = x : repetir (n-1) x

-- 4
losPrimeros :: Int -> [a] -> [a]
-- PRECOND: el número es mayor o igual que cero
losPrimeros 0 _      = []
losPrimeros _ []     = []   
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

-- 5
sinLosPrimeros :: Int -> [a] -> [a]
-- PRECOND: el número es mayor o igual que cero
sinLosPrimeros 0 xs = xs 
sinLosPrimeros _ [] = []
sinLosPrimeros n (_:xs) = sinLosPrimeros (n-1) xs


