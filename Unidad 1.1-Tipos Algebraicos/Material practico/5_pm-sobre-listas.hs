-- 2
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _  = False

-- 3
elPrimero :: [a] -> a
elPrimero (a:_) = a


-- 4
sinElPrimero :: [a] -> [a]
sinElPrimero (_:zs) = zs

-- 5
splitHead :: [a] -> (a, [a])
splitHead x = (elPrimero x, sinElPrimero x)




esSingular :: [a] -> Bool
esSingular (_:[]) = True
esSingular _      = False


tercero :: [a] -> a
tercero (_:_:x:_) = x

myList = (3:4:5:[])