type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
            | Explorador Nombre [Territorio] Lobo Lobo
            | Cria Nombre
    deriving Show
data Manada = M Lobo
    deriving Show


presas = ["reno","jabali", "conejo", "conejo", "ciervo", "bufalo"]
territorios = ["rio nilo", "bosque", "desierto"]
territorios2 = ["rio lp", "bosque","desierto"]




man = M (Cazador "Cesar" presas 
                          (Explorador "Lucas" territorios 
                                                        (Cria "Cria1")
                                                        (Cria "Cria2"))
                          (Explorador "Monito" territorios2
                                                        (Cazador "Gabi" [] 
                                                                (Cria "Cria6")
                                                                (Cria "Cria7")
                                                                (Cria "Cria8"))
                                                        (Cria "Cria3"))
                          (Cria "Cria5"))


buenaCaza :: Manada -> Bool
buenaCaza m = cantidadDeAlimento m > cantidadDeCrias m

cantidadDeAlimento :: Manada -> Int
cantidadDeAlimento (M lobo) = cantidadDeAlimentoL lobo

cantidadDeAlimentoL :: Lobo -> Int
cantidadDeAlimentoL (Cazador    _ ps l1 l2 l3) = alimentoEn ps
                                                 + cantidadDeAlimentoL l1
                                                 + cantidadDeAlimentoL l2
                                                 + cantidadDeAlimentoL l3
cantidadDeAlimentoL (Explorador _ _ l1 l2)     =   cantidadDeAlimentoL l1
                                                 + cantidadDeAlimentoL l2
cantidadDeAlimentoL (Cria _)                   = 0

alimentoEn :: [Presa] -> Int
alimentoEn ps = length ps

cantidadDeCrias :: Manada -> Int
cantidadDeCrias (M lobo) = cantidadDeCriasL lobo

cantidadDeCriasL :: Lobo -> Int
cantidadDeCriasL (Cazador   _ _ l1 l2 l3) = cantidadDeCriasL l1 
                                          + cantidadDeCriasL l2
                                          + cantidadDeCriasL l3
cantidadDeCriasL (Explorador _ _ l1 l2)   = cantidadDeCriasL l1 
                                          + cantidadDeCriasL l2
cantidadDeCriasL (Cria _)                 = 1

-------------------------------------------------

elAlfa :: Manada -> (Nombre, Int)
elAlfa (M lobo) = elAlfaL lobo

elAlfaL :: Lobo -> (Nombre,Int)
elAlfaL (Cazador    nom ps l1 l2 l3) = elegirEntre (nom, alimentoEn ps)
                                                   (elegirEntre (elAlfaL l1)
                                                                (elegirEntre (elAlfaL l2)
                                                                             (elAlfaL l3)))
elAlfaL (Explorador nom ts l1 l2)    = elegirEntre  (elAlfaL l1) 
                                                    (elegirEntre (elAlfaL l2)
                                                                 (nom, 0))
elAlfaL (Cria nom)                   = (nom, 0)



elegirEntre :: (Nombre,Int) -> (Nombre,Int) -> (Nombre,Int)
elegirEntre (nom1, c1) (nom2, c2) = if (c1 >= c2) then (nom1, c1) 
                                                  else (nom2, c2)


-------------------------------------------------

losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M lobo) = losQueExploraronL t lobo

losQueExploraronL :: Territorio -> Lobo -> [Nombre]
losQueExploraronL t (Cazador    nom _  l1 l2 l3) =  losQueExploraronL t l1
                                                 ++ losQueExploraronL t l2
                                                 ++ losQueExploraronL t l3
losQueExploraronL t (Explorador nom ts l1 l2)    =  singularSi nom (fueExplorado t ts) 
                                                 ++ losQueExploraronL t l1
                                                 ++ losQueExploraronL t l2
losQueExploraronL t (Cria _)                     = []


singularSi x b = if b then [x] else []

fueExplorado :: Territorio -> [Territorio] -> Bool
fueExplorado t ts = elem t ts

-------------------------------------------------
exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M lobo) = exploradoresPorTerritorioL lobo  


exploradoresPorTerritorioL :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorioL (Cazador _ _ l1 l2 l3)    =  
   juntarSubManadas (exploradoresPorTerritorioL l1)
                    (juntarSubManadas (exploradoresPorTerritorioL l2)
                                      (exploradoresPorTerritorioL l3))

exploradoresPorTerritorioL (Explorador nom ts l1 l2) =  
   juntarSubManadas (territoriosExplorados ts nom)
                    (juntarSubManadas (exploradoresPorTerritorioL l1)
                                      (exploradoresPorTerritorioL l2))


exploradoresPorTerritorioL (Cria nom) = []



territoriosExplorados :: [Territorio] -> Nombre -> [(Territorio, [Nombre])]
territoriosExplorados []     _   = []
territoriosExplorados (t:ts) nom = (t, [nom]) : territoriosExplorados ts nom

{-

juntarSubManadas :: [(Territorio, [Nombre])] ->  [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
juntarSubManadas []           ys            = ys
juntarSubManadas xs           []            = xs
juntarSubManadas ((t,ns):xs)  ((t',ns'):ys) = if(t == t')
                                                then (t', juntarNombres ns ns') : juntarSubManadas xs ys
                                                else [(t, ns),(t',ns')] ++ juntarSubManadas xs ys

-}


juntarSubManadas :: [(Territorio, [Nombre])] ->  [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
juntarSubManadas []           ys = []
juntarSubManadas ((t,ns):xs)  ys = if(territorioEstaEn t ys)
                                     then (agregarNombresEn t ns ys)  ++ juntarSubManadas xs ys
                                     else (t, ns) : juntarSubManadas xs ys



territorioEstaEn :: Territorio -> [ (Territorio, [Nombre]) ] -> Bool
territorioEstaEn t []      = False
territorioEstaEn t (y:ys)  = (t == fst y) || territorioEstaEn t ys

agregarNombresEn :: Territorio -> [Nombre] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarNombresEn t ns []            = []
agregarNombresEn t ns ((t',ns'):ys) = if(t == t')
                                        then (t', juntarNombres ns ns') : ys
                                        else (t',ns') : agregarNombresEn t ns ys



juntarNombres :: [Nombre] -> [Nombre] -> [Nombre]
juntarNombres []     ns' = ns'
juntarNombres (n:ns) ns' = if(elem n ns')
                             then juntarNombres ns ns'
                             else n : (juntarNombres ns ns')


-------------------------------------------------
superioresDelCazador :: Nombre -> Manada -> [Nombre]
-- PRECOND: hay un cazador con dicho nombre y es único.
superioresDelCazador nom (M lobo) = superioresDelCazadorL nom lobo


superioresDelCazadorL :: Nombre -> Lobo -> [Nombre]
superioresDelCazadorL nom (Cazador n _ l1 l2 l3) = 
    if (nom == n)
        then []
        else n : (  superioresDelCazadorL nom l1
                 ++ superioresDelCazadorL nom l2
                 ++ superioresDelCazadorL nom l3)                                                   
superioresDelCazadorL nom (Explorador n ts l1 l2) =  superioresDelCazadorL nom l1
                                                  ++ superioresDelCazadorL nom l2
superioresDelCazadorL nom (Cria n) = [] 