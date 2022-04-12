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
camino1 = Nada 
            (Cofre [Cacharro] 
                           (Cofre [Cacharro, Tesoro] Fin)
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
--REVISAR
pasosHastaTesoro :: Camino -> Int
-- PRECOND: Existe al menos un cofre tesoro
pasosHastaTesoro Fin              = 0
pasosHastaTesoro (Nada  cam)      = pasosHastaTesoro cam
pasosHastaTesoro (Cofre objs cam) = 
    (unoSi (hayAlgunTesoro objs)) + pasosHastaTesoro cam


                                    