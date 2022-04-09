-- 1
-- a)
loMismo :: a -> a
loMismo a = a

-- b)
siempreSiete :: a -> Int
siempreSiete a = 7

-- c)
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

{-
Existen dos variables de tipo diferentes porque en las tuplas el tipo de cada componente puede variar,
no es necesario que sean iguales
-}


{-
2. Las funciones anteriores son polimorficas porque nos importa la estructura de la misma y para nada
   su tipo de dato.
-}


