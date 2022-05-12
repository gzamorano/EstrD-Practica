-- Ejercicio 4 creación interfaz Empresa
import Map
import Set
import Empleado


type SectorId = Int
type CUIL = Int
data Empresa = ConsE (Map SectorId (Set Empleado))
                     (Map CUIL Empleado)



-- Propósito: construye una empresa vacía.
-- Costo: O(1)
consEmpresa :: Empresa