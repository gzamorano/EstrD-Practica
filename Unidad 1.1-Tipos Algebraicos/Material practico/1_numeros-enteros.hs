-- 1
-- a)
sucesor :: Int -> Int
sucesor n = n+1

-- b)
sumar :: Int -> Int -> Int
sumar n m = n+m

-- c)
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (div n m, mod n m)

-- d)
maxDelPar :: (Int, Int) -> Int
maxDelPar (n, m) = if(n>m)
                    then n
                    else m

-- 2
-- todas las expresiones a continuación denotan el número 10
{- 
sucesor (maxDelPar (sumar 5 4, 6))
maxDelPar(divisionYResto (sumar 16 4) (sucesor (sumar 0 1)) )
-}

