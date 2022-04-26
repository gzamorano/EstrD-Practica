
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show
data Sector = S SectorId [Componente] [Tripulante]
    deriving Show
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show
data Nave = N (Tree Sector)
    deriving Show


nave = N (NodeT (S "B37" [LanzaTorpedos,Motor 100,almacen2] ["t3"]) 
                        (NodeT (S "F14" [Motor 95,almacen1] ["t1","t4","t5"])
                                      EmptyT
                                      EmptyT) 
                        EmptyT)

almacen1 = Almacen [Oxigeno,Torpedo,Comida] 
almacen2 = Almacen [Combustible,Oxigeno]

sectores :: Nave -> [SectorId]
sectores (N t) = sectoresT t

sectoresT :: Tree Sector -> [SectorId]
sectoresT (EmptyT)          = []
sectoresT (NodeT s t1 t2) = idSector s : sectoresT t1 ++ sectoresT t2 

idSector :: Sector -> SectorId
idSector (S sId _ _) = sId

------------------------------------------------------------
poderDePropulsion :: Nave -> Int
poderDePropulsion (N t) = poderDePropulsionT t 

poderDePropulsionT :: Tree Sector -> Int
poderDePropulsionT EmptyT          = 0
poderDePropulsionT (NodeT s t1 t2) = poderDePropulsionS s + poderDePropulsionT t1 + poderDePropulsionT t2

poderDePropulsionS :: Sector -> Int
poderDePropulsionS (S _ cs _) = poderDePropulsionCs cs

poderDePropulsionCs :: [Componente] -> Int
poderDePropulsionCs []     = 0
poderDePropulsionCs (c:cs) = poderDePropulsionC c + poderDePropulsionCs cs 

poderDePropulsionC :: Componente -> Int
poderDePropulsionC (Motor n) = n
poderDePropulsionC _         = 0

------------------------------------------------------------
barriles :: Nave -> [Barril]
barriles (N t) = barrilesT t

barrilesT :: Tree Sector -> [Barril]
barrilesT EmptyT          = []
barrilesT (NodeT s t1 t2) = barrilesS s ++ barrilesT t1 ++ barrilesT t2

barrilesS :: Sector -> [Barril]
barrilesS (S _ cs _) = barrilesCs cs

barrilesCs :: [Componente] -> [Barril]
barrilesCs []     = []
barrilesCs (c:cs) = barrilesC c ++ barrilesCs cs

barrilesC :: Componente -> [Barril]
barrilesC (Almacen bs) = bs
barrilesC _            = []

------------------------------------------------------------
compsAAgregar = [Motor 30, LanzaTorpedos]

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs idS (N t) =  N (agregarASectorT cs idS t)

agregarASectorT :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASectorT cs idS EmptyT          = EmptyT
agregarASectorT cs idS (NodeT s t1 t2) = if (idS == idSector s) 
                                            then NodeT (agregarCsASector cs s) t1 t2
                                            else NodeT s 
                                                       (agregarASectorT cs idS t1)
                                                       (agregarASectorT cs idS t2)                 

agregarCsASector :: [Componente] -> Sector -> Sector
agregarCsASector cs (S idS cs' ts) = (S idS (cs ++ cs') ts)  

------------------------------------------------------------
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
-- PRECOND: todos los id de la lista existen en la nave.
asignarTripulanteA tr secs (N t) = N (asignarTripulanteT tr secs t)

asignarTripulanteT :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteT tr _    EmptyT          = EmptyT
asignarTripulanteT tr []   (NodeT s t1 t2) = NodeT s t1 t2
asignarTripulanteT tr secs (NodeT s t1 t2) = if(elem (idSector s) secs)
                                                then NodeT (asignarTripulanteS tr s) 
                                                                    (asignarTripulanteT tr secs t1) 
                                                                    (asignarTripulanteT tr secs t2)
                                                else NodeT s 
                                                           (asignarTripulanteT tr secs t1)
                                                           (asignarTripulanteT tr secs t2)
    
asignarTripulanteS :: Tripulante -> Sector -> Sector
asignarTripulanteS tr (S idS cs trs) = (S idS cs (tr:trs))

------------------------------------------------------------
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados tr (N t) = sectoresAsignadosT tr t

sectoresAsignadosT :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosT tr EmptyT          = []
sectoresAsignadosT tr (NodeT s t1 t2) = 
        singularSi (idSector s) (tripulanteEstaEn tr s) ++ sectoresAsignadosT tr t1 
                                                        ++ sectoresAsignadosT tr t2


singularSi x True  = [x]
singularSi _ False = []


tripulanteEstaEn :: Tripulante -> Sector -> Bool
tripulanteEstaEn tr (S _ _ trs) = elem tr trs

------------------------------------------------------------
tripulantes :: Nave -> [Tripulante]
tripulantes (N t) = tripulantesT t

tripulantesT :: Tree Sector -> [Tripulante]
tripulantesT EmptyT          = []
tripulantesT (NodeT s t1 t2) = agregarTripulantes (tripulantesS s) (tripulantesT t1 ++ tripulantesT t2)

tripulantesS :: Sector -> [Tripulante]
tripulantesS (S _ _ trs) = trs

agregarTripulantes :: [Tripulante] -> [Tripulante] -> [Tripulante]
agregarTripulantes []         trs2 = trs2
agregarTripulantes (tr1:trs1) trs2 = if(elem tr1 trs2)
                                        then agregarTripulantes trs1 trs2
                                        else tr1 : (agregarTripulantes trs1 trs2)