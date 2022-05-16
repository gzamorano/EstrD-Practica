-- Ejercicio 6.2
import MultiSet1
import Map1

-- (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList)

-- O(n^2) siendo n el tamaño del string dado y asumiendo que contar es de costo cuadrátrico
ocurrencias :: String -> MultiSet Char
ocurrencias cs = contar cs

-- O(n^2) siendo n la cantidad de caracteres de la lista, asumiendo que addMS es de costo cuadrático
contar :: [Char] -> MultiSet Char
contar []     = emptyMS
contar (c:cs) = addMS c (contar cs) 





