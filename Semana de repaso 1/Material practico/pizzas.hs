data Pizza = Prepizza
            | Capa Ingrediente Pizza
    deriving Show
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int | Anana | AzucarNegra | Cerezas
    deriving Show


pizza0 = Prepizza
pizza1 = Capa Salsa Prepizza
pizza2 = Capa Queso (Capa Salsa Prepizza)
pizza3 = Capa (Aceitunas 8) 
              (Capa Queso (Capa Salsa Prepizza))
pizza4 = Capa Cerezas 
              (Capa AzucarNegra (Capa Anana 
                                        (Capa Queso (Capa Jamon Prepizza))))


cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza     = 0
cantidadDeCapas (Capa ing p) = 1 + cantidadDeCapas p

-------------------------------------------------------------
armarPizza :: [Ingrediente] -> Pizza
armarPizza []         = Prepizza 
armarPizza (ing:ings) = Capa ing (armarPizza ings)  

-------------------------------------------------------------
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza     = Prepizza
sacarJamon (Capa ing p) = if (esJamon ing)
                            then sacarJamon p
                            else Capa ing (sacarJamon p)
    
esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _     = False

-------------------------------------------------------------
--tieneSoloSalsaYQueso :: Pizza -> Bool
-- Dice si una pizza tiene salsa y queso REVISAR, tratar de hacer varias soluciones
--tieneSoloSalsaYQueso Prepizza     = 
--tieneSoloSalsaYQueso (Capa ing p) = (esSalsaOQueso ing) ... tieneSoloSalsaYQueso p


esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = False
esSalsaOQueso Queso = False
esSalsaOQueso _     = True


-------------------------------------------------------------
duplicarAceitunas :: Pizza -> Pizza
--Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas Prepizza     = Prepizza
duplicarAceitunas (Capa ing p) = if (esAceituna ing)
                                    then Capa (duplicarCantAceitunas ing) (duplicarAceitunas p)
                                    else Capa ing (duplicarAceitunas p)

esAceituna (Aceitunas _) = True
esAceituna _          = False

duplicarCantAceitunas (Aceitunas n) = Aceitunas (n*2)

-------------------------------------------------------------
cantCapasPorPizza :: [Pizza] -> [ (Int, Pizza) ]
--Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
--ingredientes de la pizza, y la respectiva pizza como segunda componente.
cantCapasPorPizza []     = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p, p) : cantCapasPorPizza ps


