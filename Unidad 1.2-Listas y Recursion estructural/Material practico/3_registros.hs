-- 1
type Nombre = String
type Edad   = Int

data Persona = P Nombre Edad
    deriving Show

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ []     = []
mayoresA n (p:ps) = if(esMayorA n p)
                        then p : mayoresA n ps
                        else mayoresA n ps

esMayorA :: Int -> Persona -> Bool
esMayorA n p = edad p > n

gonzalo = P "Gonzalo" 21
jorge   = P "Jorge"   25

-----------------------------------------------------------------

promedioEdad :: [Persona] -> Edad
-- PRECOND: la lista contiene al menos una Persona
promedioEdad ps = div (sumaDeEdades (todasLasEdades ps)) (length ps)  

sumaDeEdades :: [Edad] -> Int
sumaDeEdades ns = sum ns

todasLasEdades :: [Persona] -> [Edad]
todasLasEdades []     = [] 
todasLasEdades (p:ps) = edad p : todasLasEdades ps 

-----------------------------------------------------------------

elMasViejo :: [Persona] -> Persona
-- PRECOND: la lista contiene al menos una Persona
elMasViejo (p:[]) = p
elMasViejo (p:ps) = laQueEsMayor p (elMasViejo ps) 


esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = (edad p1) > (edad p2)


-- 2

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
                                      -- Energia
data Entrenador = ConsEntrenador String [Pokemon]
                              -- Nombre

charizard  = ConsPokemon Fuego 95
squirtle   = ConsPokemon Agua  87
charmander = ConsPokemon Fuego 90
bulbasaur  = ConsPokemon Planta 78

ash = ConsEntrenador "Ash" [charizard]
hsa = ConsEntrenador "Sha" [squirtle, bulbasaur]

cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ ps) = length ps

-----------------------------------------------------------------

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador _ ps) = cantPokemonesDelTipoEn t ps

cantPokemonesDelTipoEn :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonesDelTipoEn _ []     = 0
cantPokemonesDelTipoEn t (p:ps) = (if sonDelMismoTipo t (tipo p) then 1 else 0) 
                                  + (cantPokemonesDelTipoEn t ps) 

-----------------------------------------------------------------

losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan t (ConsEntrenador _ ps) (ConsEntrenador _ ps1) =  pokemonQueLeGanan t ps ps1

-- Cada pokemon del tipo dado del primer entrenador le tiene que ganar a todos los 
-- pokemons del segundo entrenador
pokemonQueLeGanan :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
pokemonQueLeGanan t []     _   = 0  
pokemonQueLeGanan t (p:ps) ps1 = (if(superaATodos t p ps1) then 1 else 0)
                                 + (pokemonQueLeGanan t ps ps1) 

superaATodos :: TipoDePokemon -> Pokemon -> [Pokemon] -> Bool
superaATodos t p []       = False
superaATodos t p (p1:[])  = superaA p p1
superaATodos t p (p1:ps1) = superaA p p1 && superaATodos t p ps1

-----------------------------------------------------------------

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ ps) = hayDeTodosLosTiposEn ps

hayDeTodosLosTiposEn :: [Pokemon] -> Bool
hayDeTodosLosTiposEn ps = (hayDeTipoEn Fuego ps) && (hayDeTipoEn Planta ps) && (hayDeTipoEn Agua ps)

hayDeTipoEn :: TipoDePokemon -> [Pokemon] -> Bool
hayDeTipoEn t []     = False
hayDeTipoEn t (p:ps) = sonDelMismoTipo t (tipo p) || hayDeTipoEn t ps 


-- 3

data Seniority = Junior | SemiSenior | Senior
    deriving Show
data Proyecto = ConsProyecto String
    deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
    deriving Show
data Empresa = ConsEmpresa [Rol]
    deriving Show

-- creación de variables para testear
web     = ConsProyecto "WebPage"
desktop = ConsProyecto "DesktopApp" 

dev1 = Developer Junior web
dev2 = Developer Senior desktop
dev3 = Developer Senior web
dev4 = Developer Senior web
man1 = Management SemiSenior desktop
man2 = Management SemiSenior web

unaEmpresa = ConsEmpresa [dev1,dev2,dev3,dev4,man1,man2]

proyectos ::  Empresa ->  [Proyecto]
proyectos (ConsEmpresa rs)  = sinLosRepetidos (losProyectosDe rs)

sinLosRepetidos ::  [Proyecto] -> [Proyecto]
sinLosRepetidos []     = []
sinLosRepetidos (p:ps) = if(proyectoPerteneceA p ps)
                            then sinLosRepetidos ps
                            else p : sinLosRepetidos ps 

proyectoPerteneceA :: Proyecto -> [Proyecto] -> Bool
proyectoPerteneceA p []      = False
proyectoPerteneceA p (p':ps) = proyectosSonIguales p p' || proyectoPerteneceA p ps

losProyectosDe :: [Rol] -> [Proyecto]
losProyectosDe []     = []
losProyectosDe (r:rs) = proyectoDe r : losProyectosDe rs

proyectoDe :: Rol -> Proyecto
proyectoDe (Developer  _ p) = p
proyectoDe (Management _ p) = p

proyectosSonIguales :: Proyecto -> Proyecto -> Bool
proyectosSonIguales p p' =  nombreProyecto p == nombreProyecto p'

nombreProyecto :: Proyecto -> String
nombreProyecto (ConsProyecto n) = n

-----------------------------------------------------------------

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa rs) ps = cantidadDeDevSeniorEn (losDevQuePertenecenA rs ps)

cantidadDeDevSeniorEn :: [Rol] -> Int
cantidadDeDevSeniorEn []     = 0
cantidadDeDevSeniorEn (r:rs) = (if esDevSenior r then 1 else 0)
                              + (cantidadDeDevSeniorEn rs)

esDevSenior :: Rol -> Bool
esDevSenior (Developer s _) = sonDelMismoSeniority s Senior

sonDelMismoSeniority :: Seniority -> Seniority -> Bool
sonDelMismoSeniority Senior     Senior     = True
sonDelMismoSeniority SemiSenior SemiSenior = True
sonDelMismoSeniority Junior     Junior     = True
sonDelMismoSeniority _          _          = False

losDevQuePertenecenA :: [Rol] -> [Proyecto] -> [Rol]
losDevQuePertenecenA []     _  = []
losDevQuePertenecenA (r:rs) ps = if(esDev r && perteneceAAlgunoEn r ps)
                                                then r : losDevQuePertenecenA rs ps
                                                else losDevQuePertenecenA rs ps

esDev :: Rol -> Bool
esDev (Management _ _) = False
esDev (Developer _ _)  = True

perteneceAAlgunoEn :: Rol -> [Proyecto] -> Bool
perteneceAAlgunoEn (Developer  _ p) ps = proyectoPerteneceA p ps
-- función proyectoPerteneceA definida en el ejercicio anterior


-----------------------------------------------------------------

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn []     _ = 0
cantQueTrabajanEn (p:ps) e = if(perteneceALaEmpresa p e)
                                then (cantDeEmpleadosEn p e) + cantQueTrabajanEn ps e
                                else cantQueTrabajanEn ps e 

perteneceALaEmpresa :: Proyecto -> Empresa -> Bool
perteneceALaEmpresa p e = proyectoPerteneceA p (proyectos e)

cantDeEmpleadosEn :: Proyecto -> Empresa -> Int
cantDeEmpleadosEn p (ConsEmpresa rs) = length (losQueTrabajanEn p rs)

losQueTrabajanEn :: Proyecto -> [Rol] -> [Rol]
losQueTrabajanEn p []     = []
losQueTrabajanEn p (r:rs) = if(trabajaEn r p)
                                then r : losQueTrabajanEn p rs
                                else losQueTrabajanEn p rs

trabajaEn :: Rol -> Proyecto -> Bool
trabajaEn r p = proyectosSonIguales (proyectoDe r) p


-----------------------------------------------------------------

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto e = asignadosPorProyecto' (proyectos e)  e

asignadosPorProyecto' :: [Proyecto] -> Empresa -> [(Proyecto, Int)]
asignadosPorProyecto' []     _ = []
asignadosPorProyecto' (p:ps) e = (p,cantDeEmpleadosEn p e) : asignadosPorProyecto' ps e








-- funciones de la parte 1 de esta misma práctica
edad :: Persona -> Int
edad (P _ e) = e 

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if(esMayorQueLaOtra p1 p2)
                      then p1
                      else p2


sonDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonDelMismoTipo Agua   Agua   = True
sonDelMismoTipo Fuego  Fuego  = True
sonDelMismoTipo Planta Planta = True
sonDelMismoTipo _      _      = False


superaA :: Pokemon -> Pokemon -> Bool
-- En esta función se trabaja sobre Pokemon
superaA p1 p2 = tipoSuperiorA (tipo p1) (tipo p2)

tipoSuperiorA :: TipoDePokemon -> TipoDePokemon -> Bool
-- En esta otra función se trabaja sobre TipoDePokemon, así dividimos en subtareas
tipoSuperiorA Agua   Fuego  = True
tipoSuperiorA Fuego  Planta = True
tipoSuperiorA Planta Agua   = True
tipoSuperiorA _      _      = False

-- Se genera la función obsevadora para saber el tipo del pokemon
tipo :: Pokemon -> TipoDePokemon
tipo (ConsPokemon t _) = t

