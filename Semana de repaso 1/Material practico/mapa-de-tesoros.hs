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
                            (Fin (Cofre [Tesoro])) 
                            (Bifurcacion (Cofre [Chatarra]) 
                                                    map0 
                                                    map0) 

                  

        
hayTesoro :: Mapa -> Bool
-- indica si hay tesoro en alguna parte del mapa
hayTesoro (Fin c)               = hayAlgunTesoro c
hayTesoro (Bifurcacion c m1 m2) = hayAlgunTesoro c || hayTesoro m1 || hayTesoro m2


hayAlgunTesoro :: Cofre -> Bool
hayAlgunTesoro (Cofre [])         = False
hayAlgunTesoro (Cofre (obj:objs)) = esTesoro obj || hayAlgunTesoro (Cofre objs)

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

------------------------------------------------------------

hayTesoroEn :: [Dir] -> Mapa -> Bool
--Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
--lista vacía de direcciones.
hayTesoroEn []     m = 
hayTesoroEn (d:ds) m = hayTesoroEn ds m


