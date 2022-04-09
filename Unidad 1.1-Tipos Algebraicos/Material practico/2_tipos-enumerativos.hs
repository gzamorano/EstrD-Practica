-- 1
data Dir = Norte | Este | Sur | Oeste
     deriving Show
-- a)
opuesto :: Dir -> Dir
-- con Pattern Matching
opuesto Norte = Sur
opuesto Este  = Oeste 
opuesto Sur   = Norte
opuesto Oeste = Este
    
-- con case of (Alternativa indexada)
{-
opuesto d = 
    case d of
        Norte -> Sur
        Este  -> Oeste
        Sur   -> Norte
        Oeste -> Este
-}

-- b)
iguales :: Dir -> Dir -> Bool
iguales Norte Norte  = True 
iguales Este  Este   = True
iguales Sur   Sur    = True
iguales Oeste Oeste  = True
iguales _     _      = False


-- c) 
siguiente :: Dir -> Dir
-- PRECOND: Oeste no puede ser argumento de la funcion
siguiente Norte = Este
siguiente Este  = Sur 
siguiente Sur   = Oeste 

-- 2
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
     deriving Show

-- a)
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

-- b)
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes    = True 
empiezaConM Miercoles = True
empiezaconM _         = False

-- c)
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Martes    Lunes     = True
vieneDespues Miercoles Martes    = True
vieneDespues Jueves    Miercoles = True
vieneDespues Viernes   Jueves    = True
vieneDespues Sabado    Viernes   = True
vieneDespues Domingo   Sabado    = True
vieneDespues Lunes     Domingo   = True
vieneDespues _         _         = False

-- d)
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes   = False
estaEnElMedio Domingo = False
estaEnElMedio _       = True

-- 3
-- a)
negar :: Bool -> Bool
negar True  = False
negar False = True

-- b)
implica :: Bool -> Bool -> Bool
implica True False = False
implica _    _     = True

-- c)
and :: Bool -> Bool -> Bool
and True True = True
and _    _    = False

-- d)
or :: Bool -> Bool -> Bool
or True  False = True
or False True  = True
or _     _     = False




