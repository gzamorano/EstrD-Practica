-- Ejercicio 4 creación interfaz Empresa
module Empresa 
    (consEmpresa)
where

import Map
import Set
import Empleado



type SectorId = Int
type CUIL = Int
data Empresa = ConsE (Map SectorId (Set Empleado))
                     (Map CUIL Empleado)

{-
    INV.REP.: en ConsE sects empls
    * en sects y empls no existen claves repetidas
    * ?
-}


{-
los empleados son un tipo abstracto.
el primer map relaciona id de sectores con los empleados que trabajan en dicho sector.
el segundo map relaciona empleados con su número de CUIL.
un empleado puede estar asignado a más de un sector
tanto Map como Set exponen una interfaz eciente con costos logarítmicos para inserción,
búsqueda y borrado, tal cual vimos en clase.

El tipo abstracto Map ya me asegura que no van a haber Cuils repetidos. Lo mismo sucede
con los sectores, no van a haber repetidos y en un mismo sector no existiran dos empleados
iguales, ya que esto lo garantiza el tipo abstracto Set.
-}



-- Propósito: construye una empresa vacía.
-- Costo: O(1)
consEmpresa :: Empresa
consEmpresa = ConsE emptyM emptyM

-- =================================================================

-- Propósito: devuelve el empleado con dicho CUIL.
-- Costo: O(log E) siendo E la cantidad de empleados de la empresa.
buscarPorCUIL :: CUIL -> Empresa -> Empleado
buscarPorCUIL c (ConsE _ empls) = 
    case (lookupM c empls) of 
        (Just x) -> x
        Nothing  -> error "no existe empleado con tal CUIL en la empresa"

-- =================================================================

-- Propósito: indica los empleados que trabajan en un sector dado.
-- Costo: O(logS + E)
empleadosDelSector :: SectorId -> Empresa -> [Empleado]
empleadosDelSector sId (ConsE sects _) = empleadosDelSectorM sId sects

-- Costo: O(logS + E) siendo S la cantidad de sectores y E de empleados. 
-- se busca el sector dado y luego hay que recorrer el Set para listar 
-- los empleados de tal sector.
empleadosDelSectorM :: SectorId -> Map SectorId (Set Empleado) -> [Empleado]
empleadosDelSectorM sId sects =  
    let empleados = lookupM sId sects
        in  case empleados of
                (Just x) -> empleados x
                Nothing  -> [] 

-- Costo: O(E) siendo E la cantidad de empleados, y asumiendo que setToList tiene costo
-- lineal.
empleados :: Set Empleado -> [Empleado]
empleados empls = setToList empls

-- =================================================================

-- Propósito: indica todos los CUIL de empleados de la empresa.
-- Costo: O(E) siendo E la cantidad de empleados de la empresa, asumiendo que op. keys
-- tiene costo lineal.
todosLosCUIL :: Empresa -> [CUIL]
todosLosCUIL (ConsE _ empls) = keys empls

-- =================================================================

-- Propósito: indica todos los sectores de la empresa.
-- Costo: O(S) siendo S la cantidad de sectores de la empresa, asumiendo que la op. keys
-- tiene costo lineal, ya que recorre TODO el Map.
todosLosSectores :: Empresa -> [SectorId]
todosLosSectores (ConsE sects _) = keys sects


-- =================================================================

-- Propósito: agrega un sector a la empresa, inicialmente sin empleados.
-- Costo: O(logS)
agregarSector :: SectorId -> Empresa -> Empresa
agregarSector sId (ConsE sects empls) = 
    case (lookupM sId sects) of 
        (Just _) -> ConsE sects empls -- en este caso no agrega el sector, porque ya existe uno con esa Id
        Nothing  -> ConsE (agregarSectorM sId sects) empls

-- Costo: O(logS) siendo S la cantidad de sectores 
agregarSectorM :: SectorId -> Map SectorId (Set Empleado) ->  Map SectorId (Set Empleado)
agregarSectorM sId sects = assocM sId emptyS sects 

-- =================================================================

-- Propósito: agrega un empleado a la empresa, en el que trabajará en dichos sectores y tendrá el CUIL dado.
-- Costo: calcular
agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
agregarEmpleado sIds cuil (ConsE sects empls) =  
    let nuevoEmpleado = empleadoCon sIds CUIL  
        in ...aca construyo la empresa

empleadoCon :: [SectorId] -> CUIL -> Empleado
empleadoCon sIds cuil = agregarSectores sIds (consEmpleado cuil)


agregarSectores :: [SectorId]  -> Empleado -> Empleado
agregarSectores = undefined

{-
EMPRESA

RRHH: rober,juan
DEV: mati,juli

222: rober
333: juan
444: mati
555: juli



-}