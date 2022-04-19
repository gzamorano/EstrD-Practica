-- 1.1 Celdas con bolitas

data Color = Azul | Rojo 
    deriving (Eq, Show) 
      
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

celda0 = CeldaVacia
celda1 = Bolita Rojo 
                (Bolita Azul 
                    (Bolita Rojo 
                        (Bolita Azul CeldaVacia)
                    )
                )
-- la operación que ayuda a resolver el problema es la función: apariciones, que ya definimos
nroBolitas :: Color -> Celda -> Int
nroBolitas c CeldaVacia      = 0
nroBolitas c (Bolita c' cel) = unoSi (sonDelMismoColor c c') + nroBolitas c cel

unoSi :: Bool -> Int
unoSi b = if b then 1 else 0

sonDelMismoColor :: Color -> Color -> Bool
sonDelMismoColor c c' = c == c'

-----------------------------------------------------------
celda2 = Bolita Azul CeldaVacia


poner :: Color -> Celda -> Celda
-- poner Rojo celda2 -> Bolita Rojo (Bolita Azul CeldaVacia)
poner c CeldaVacia      = Bolita c CeldaVacia
poner c (Bolita c' cel) = Bolita c (poner c' cel)

-----------------------------------------------------------
celda3 = Bolita Rojo 
                (Bolita Rojo 
                        (Bolita Azul CeldaVacia)
                )

-- Va a sacar la primer bolita que encuentre del color dado
sacar :: Color -> Celda -> Celda
--sacar Azul celda2 -> CeldaVacia
sacar c CeldaVacia      = CeldaVacia
sacar c (Bolita c' cel) = if(sonDelMismoColor c c') 
                            then cel
                            else Bolita c' (sacar c cel)

-----------------------------------------------------------

ponerN :: Int -> Color -> Celda -> Celda
--ponerN 2 Rojo celda2 -> Bolita Rojo (Bolita Rojo (Bolita Azul CeldaVacia))
ponerN 0 c cel = cel
ponerN n c cel = poner c (ponerN (n-1) c cel)


-- 1.2 Camino hacia el tesoro

data Objeto = Cacharro | Tesoro
    deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show

camino0 = Fin
camino1 = Nada (Cofre [Tesoro, Cacharro,Tesoro,Tesoro] 
                        (Cofre [Cacharro] 
                                    (Cofre [Tesoro] Fin)
                        )
                )

camino2 = (Cofre [Cacharro, Tesoro] Fin)

hayTesoro :: Camino -> Bool
hayTesoro Fin              = False
hayTesoro (Nada  cam)      = hayTesoro cam
hayTesoro (Cofre objs cam) = hayAlgunTesoro objs || hayTesoro cam

hayAlgunTesoro :: [Objeto] -> Bool
hayAlgunTesoro []         = False
hayAlgunTesoro (obj:objs) = esTesoro obj || hayAlgunTesoro objs 

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

-----------------------------------------------------------
pasosHastaTesoro :: Camino -> Int
-- PRECOND: Existe al menos un cofre tesoro
pasosHastaTesoro Fin              = 0
pasosHastaTesoro (Nada  cam)      = 1 + pasosHastaTesoro cam
pasosHastaTesoro (Cofre objs cam) =  if(hayAlgunTesoro objs)
                                        {- Si uso unoSi sería lo mismo que poner..
                                         then 0 + pasosHastaTesoro cam
                                         seguiría entonces contando los pasos hasta los demás tesoros, 
                                         no es lo que se busca
                                        -}
                                        then 0
                                        else 1 + pasosHastaTesoro cam

-----------------------------------------------------------
hayTesoroEn :: Int -> Camino -> Bool
-- PRECOND: el camino tiene al menos n pasos para recorrer
hayTesoroEn _ Fin              = False
hayTesoroEn n (Nada cam)       = if(n == 0) 
                                    then False
                                    else hayTesoroEn (n-1) cam
hayTesoroEn n (Cofre objs cam) = if(n == 0)
                                    then hayAlgunTesoro objs
                                    else hayTesoroEn (n-1) cam

----------------------------------------------------------- 
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n cam = (cantDeTesorosEn cam) >= n


cantDeTesorosEn :: Camino -> Int
cantDeTesorosEn Fin              = 0
cantDeTesorosEn (Nada cam)       = cantDeTesorosEn cam
cantDeTesorosEn (Cofre objs cam) = tesorosEn objs + cantDeTesorosEn cam

tesorosEn :: [Objeto] -> Int
tesorosEn []         = 0
tesorosEn (obj:objs) = unoSi (esTesoro obj) + tesorosEn objs

-----------------------------------------------------------
cantTesorosEntre :: Int -> Int -> Camino -> Int
-- PRECOND: el segundo número dado no puede ser menor que el primero
cantTesorosEntre n n1 Fin              = 0
cantTesorosEntre n n1 (Nada cam)       = cantTesorosEntre (n-1) (n1-1) cam
cantTesorosEntre n n1 (Cofre objs cam) = if (n <= 0 && n1 >= 0)
                                            then tesorosEn objs + cantTesorosEntre (n-1) (n1-1) cam
                                            else cantTesorosEntre (n-1) (n1-1) cam