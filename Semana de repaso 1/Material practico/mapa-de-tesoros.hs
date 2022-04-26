data Dir    = Izq | Der
    deriving Show
data Objeto = Tesoro | Chatarra
    deriving Show
data Cofre  = Cofre [Objeto]
    deriving Show
data Mapa   = Fin Cofre
            | Bifurcacion Cofre Mapa Mapa
    deriving Show

map0 = Fin (Cofre [Chatarra])

map1 = Bifurcacion (Cofre [Chatarra]) 
                            (Fin (Cofre [Chatarra])) 
                            (Bifurcacion (Cofre [Chatarra]) 
                                                    (Fin (Cofre [Tesoro]))
                                                    map0) 

                  

        
hayTesoro :: Mapa -> Bool
-- indica si hay tesoro en alguna parte del mapa
hayTesoro (Fin c)               = hayAlgunTesoro c
hayTesoro (Bifurcacion c m1 m2) = hayAlgunTesoro c || hayTesoro m1 || hayTesoro m2

hayAlgunTesoro :: Cofre -> Bool
hayAlgunTesoro (Cofre objs) = hayAlgunTesoroObjs objs

hayAlgunTesoroObjs :: [Objeto] -> Bool
hayAlgunTesoroObjs []         = False
hayAlgunTesoroObjs (obj:objs) = esTesoro obj || hayAlgunTesoroObjs objs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

------------------------------------------------------------

hayTesoroEn :: [Dir] -> Mapa -> Bool
--Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
--lista vacía de direcciones.
hayTesoroEn []     (Fin c)               = hayAlgunTesoro c
hayTesoroEn _      (Fin c)               = error "No es posible llegar al final del camino"
hayTesoroEn []     (Bifurcacion c _  _)  = hayAlgunTesoro c
hayTesoroEn (d:ds) (Bifurcacion _ m1 m2) = 
    if (esIzq d)
      then hayTesoroEn ds m1 
      else hayTesoroEn ds m2 


esIzq Izq = True
esIzq _   = False

-------------------------------------------------------------

caminoAlTesoro :: Mapa -> [Dir]
-- PRECOND: existe un tesoro y es único.
caminoAlTesoro (Fin c)               = []
caminoAlTesoro (Bifurcacion c m1 m2) =  
    if (hayAlgunTesoro c)
      then [] 
      else if (hayTesoro m1)
              then Izq : caminoAlTesoro m1 
              else Der : caminoAlTesoro m2  

-------------------------------------------------------------

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin _)               = []
caminoDeLaRamaMasLarga (Bifurcacion _ m1 m2) =  
    if (laPrimeraEsMasLarga (caminoDeLaRamaMasLarga m1) (caminoDeLaRamaMasLarga m2))
        then Izq : caminoDeLaRamaMasLarga m1
        else Der : caminoDeLaRamaMasLarga m2


laPrimeraEsMasLarga xs ys = (length xs) > (length ys)

-------------------------------------------------------------
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c)               = [singularSi (objDelCofre c) (esTesoro (objDelCofre c))]
tesorosPorNivel (Bifurcacion c m1 m2) = 
    (singularSi (objDelCofre c) (esTesoro (objDelCofre c))) 
    : juntarNivelesMapa (tesorosPorNivel m1) (tesorosPorNivel m2)


juntarNivelesMapa :: [[Objeto]] -> [[Objeto]] -> [[Objeto]]
juntarNivelesMapa []             objss2         = objss2
juntarNivelesMapa objss1         []             = objss1
juntarNivelesMapa (objs1:objss1) (objs2:objss2) = (objs1 ++ objs2) : juntarNivelesMapa objss1 objss2


objDelCofre :: Cofre -> Objeto
objDelCofre (Cofre obj) = head obj

singularSi x True  = [x]
singularSi _ False = []


-------------------------------------------------------------
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin c)               = [[]]
todosLosCaminos (Bifurcacion c m1 m2) = consACada Izq (todosLosCaminos m1)
                                     ++ consACada Der (todosLosCaminos m2)


consACada :: a -> [[a]] -> [[a]]
consACada x []       = []
consACada x (xs:xss) = (x:xs) : consACada x xss




