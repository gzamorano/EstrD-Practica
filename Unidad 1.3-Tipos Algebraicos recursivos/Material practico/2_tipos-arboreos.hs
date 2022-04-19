-- 2.1 Árboles binarios

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

tree0 = EmptyT
tree1 :: Tree Int
tree1 = NodeT 2 (NodeT 5 
                  (NodeT 7 EmptyT 
                    (NodeT 1 EmptyT EmptyT)) 
                  (NodeT 8 EmptyT EmptyT)) 
                (NodeT 0 
                    (NodeT 4 EmptyT EmptyT) EmptyT)   
                                            

-- 1
sumarT :: Tree Int -> Int
sumarT EmptyT        = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2

-- 2
sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT n t1 t2) = 1 + sizeT t1 + sizeT t2

-- 3
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT n t1 t2) = NodeT (n*2) (mapDobleT t1) (mapDobleT t2)

-- 4
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT x EmptyT           = False
perteneceT x (NodeT x1 t1 t2) = (x == x1) || perteneceT x t1 || perteneceT x t2

-- 5
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT x EmptyT          = 0
aparicionesT x (NodeT x1 t1 t2) = unoSi(x == x1) + aparicionesT x t1 + aparicionesT x t2 

unoSi :: Bool -> Int
unoSi b = if b then 1 else 0

-- 6
leaves :: Tree a -> [a]
leaves EmptyT                  = []
leaves (NodeT x EmptyT EmptyT) = x:[] 
leaves (NodeT x t1     t2)     = (leaves t1) ++ (leaves t2) 

-- 7
heightT :: Tree a -> Int
heightT EmptyT                  = 0
heightT (NodeT _ EmptyT EmptyT) = 0
heightT (NodeT _ t1     t2)     = 1 + (max (heightT t1)  (heightT t2))

-- 8 
mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT x t1 t2) = NodeT x (mirrorT t2) (mirrorT t1)

-- 9 
toList :: Tree a -> [a]
toList EmptyT          = []
toList (NodeT x t1 t2) = toList t1 ++ x:[] ++ toList t2

-- 10
levelN :: Int -> Tree a -> [a]
levelN 0 (NodeT x _  _ ) = x:[]
levelN n EmptyT          = []
levelN n (NodeT x t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2 

-- ASI ES COMO DEBE HACERSE SIGUIENDO LOS PASOS DE LA RECURSION
levelN' :: Int -> Tree a -> [a]
levelN' _ EmptyT          = []
levelN' 0 (NodeT x _  _)  = x:[]
levelN' n (NodeT _ t1 t2) = levelN' (n-1) t1 ++ levelN' (n-1) t2

-- 11
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = [x] : juntarNiveles (listPerLevel t1) (listPerLevel t2)

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles []       yss      = yss
juntarNiveles xss      []       = xss
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) :  juntarNiveles xss yss


-- 12 
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x t1 t2) = x : laMayorEntre (ramaMasLarga t1)  (ramaMasLarga t2)


laMayorEntre :: [a] -> [a] -> [a]
laMayorEntre xs ys = if (length ys > length xs) then ys else xs


-- 13

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x t1 t2) =  [x] : (consACada x (todosLosCaminos t1))
                                   ++ consACada x (todosLosCaminos t2)        


consACada :: a -> [[a]] -> [[a]] 
consACada x []       = []
consACada x (xs:xss) = (x : xs) : consACada x xss



-- 2.2 Expresiones aritméticas
data ExpA = Valor Int
            | Sum ExpA ExpA
            | Prod ExpA ExpA
            | Neg ExpA
    deriving Show

exp0 = Valor 5
exp1 = Sum (Valor 2) (Valor 7)
exp2 = Sum (Prod (Valor 3) (Valor 2)) (Valor 7)
exp3 = Neg (Sum (Valor 2) (Valor 7))

-- 1
eval :: ExpA -> Int
eval (Valor n)     = n
eval (Sum   e1 e2) = (eval e1) + (eval e2)
eval (Prod  e1 e2) = (eval e1) * (eval e2)
eval (Neg   e1)    = -(eval e1) 


-- 2
simplificar :: ExpA -> ExpA
-- a) 0 + x = x + 0 = x
simplificar (Sum (Valor 0) exp)  = exp
simplificar (Sum exp (Valor 0))  = exp
-- b) 0 * x = x * 0 = 0
simplificar (Prod (Valor 0) exp) = Valor 0
simplificar (Prod exp (Valor 0)) = Valor 0
-- c) 1 * x = x * 1 = x
simplificar (Prod (Valor 1) exp) = exp
simplificar (Prod exp (Valor 1)) = exp
-- d) - (- x) = x
simplificar (Neg (Neg exp))      = exp