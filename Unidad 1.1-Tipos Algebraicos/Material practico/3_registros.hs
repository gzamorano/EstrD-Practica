-- 1
data Persona = P String Int
               -- Nombre Edad
    deriving Show

nombre :: Persona -> String
nombre (P n _) =  n

edad :: Persona -> Int
edad (P _ e) = e

crecer :: Persona -> Persona
crecer (P n e) = (P n (e+1))

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre n' (P n e) = (P n' e)

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = (edad p1) > (edad p2)

gonzalo = P "Gonzalo" 21
fidel = P "Fidel" 53

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if(esMayorQueLaOtra p1 p2)
                      then p1
                      else p2


-- 2
data Pokemon = Pokemon TipoDePokemon Int
                    -- TipoDePokemon Porcentaje energia
     deriving Show
data TipoDePokemon = Agua | Fuego | Planta
     deriving Show
data Entrenador = E String Pokemon Pokemon
                 -- Nombre Poke1   Poke2
     deriving Show

----------------------------------------
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
tipo (Pokemon t _) = t

myPoke  = Pokemon Agua 85
myPoke2 = Pokemon Agua 63

----------------------------------------

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t (E _ p1 p2) = cantidadDeTipo t p1 p2 

cantidadDeTipo :: TipoDePokemon -> Pokemon -> Pokemon -> Int
cantidadDeTipo t (Pokemon t1 _) (Pokemon t2 _) = (if(sonDelMismoTipo t t1) then 1 else 0) +
                                                 (if(sonDelMismoTipo t t2) then 1 else 0)

{-
Otra forma con la función unoSiCeroSino

cantidadDeTipo :: TipoDePokemon -> Pokemon -> Pokemon -> Int
cantidadDeTipo t (Pokemon t1 _) (Pokemon t2 _) = (unoSiCeroSino (sonDelMismoTipo t t1)) +
                                                 (unoSiCeroSino (sonDelMismoTipo t t1))
-}


unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino _    = 0 


sonDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonDelMismoTipo Agua   Agua   = True
sonDelMismoTipo Fuego  Fuego  = True
sonDelMismoTipo Planta Planta = True
sonDelMismoTipo _      _      = False


----------------------------------------

entr = E "ent1" myPoke myPoke2

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
-- En esta función se trabaja sobre Entrenadores
juntarPokemon (e1, e2) = pokemonesDe e1 ++ pokemonesDe e2

pokemonesDe :: Entrenador -> [Pokemon]
-- En esta otra función(subtarea) se trabaja sobre Pokemones
pokemonesDe (E _ p1 p2) = p1:p2:[] -- válido también [p1,p2]


---------------------------------------








