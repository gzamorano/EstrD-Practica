data Color = Azul | Rojo 
    deriving (Eq, Show) 
      
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

celda1 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))

nroBolitas :: Color -> Celda -> Int
nroBolitas c CeldaVacia      = 0
nroBolitas c (Bolita c' cel) = unoSi (esDeColor c c') + nroBolitas c cel

unoSi :: Bool -> Int
unoSi b = if b then 1 else 0

esDeColor :: Color -> Color -> Bool
esDeColor c c' = c == c'