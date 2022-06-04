-- Ejercicio 4 creación interfaz Empresa
module Empresa 
    (consEmpresa)
where

import Map1
import Set1
import Empleado



type SectorId = Int
type CUIL = Int
data Empresa = ConsE (Map SectorId (Set Empleado))
                     (Map CUIL Empleado)

{-
    INV.REP.: en ConsE ms me
    * Para toda asociacion n -> e en me, n es un número positivo de 11 dígitos.
    * Para todo sId que esté asignado a un e en me, ese sId en ms tiene en su conjunto de empleados a e.
                                                  | vale e pertenece a (empleadosDelSector sId).
    * Para toda asociación sId -> se en ms, para todo e de se, sId pertenece a los sectores asignados de e en me.
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

instance Show Empresa where 
    show (ConsE sects empls) = "Sectores: \n" ++ 
                                showSects (keys sects) sects 
                                ++ 
                                "\nEmpleados: \n" ++
                                showEmpls (keys empls) empls

       where showSects []     _     = ""
             showSects (n:ns) sects = showSect n sects ++ "\n" ++ showSects ns sects
             showSect n sects       = "   " ++ show n ++ " -> " ++ show (empleadosDelSectorM n sects)

             showEmpls []     _     = ""
             showEmpls (n:ns) empls = showEmpl n empls ++ "\n" ++ showEmpls ns empls
             showEmpl n empls = "   -" ++ show (case (lookupM n empls) of 
                                                                (Just x) -> x)  
                                                                


miEmpresa = agregarEmpleado [11] 555
          $ agregarEmpleado [10] 204310
          $ agregarEmpleado [10,11] 20421
          $ agregarEmpleado [09] 998
          $ agregarSector 10
          $ agregarSector 11
          $ agregarSector 09
          $ consEmpresa

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
                (Just x) -> empleadosS x
                Nothing  -> [] 

-- Costo: O(E) siendo E la cantidad de empleados, y asumiendo que setToList tiene costo
-- lineal.
empleadosS :: Set Empleado -> [Empleado]
empleadosS empls = setToList empls

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
-- Costo: O(S logS + logE) 
agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
agregarEmpleado sIds cuil (ConsE sects empls) =  
    let nuevoEmpleado = empleadoCon sIds cuil  
        in ConsE (agregarEmplEnSectores nuevoEmpleado sIds sects) 
                 (assocM cuil nuevoEmpleado empls)

-- Costo: O(S logS) asumiendo que S es el tamaño de la lista de id sectores, y asumiendo
-- que la op. agregarSectores es de costo S logS.
empleadoCon :: [SectorId] -> CUIL -> Empleado
empleadoCon sIds cuil = agregarSectores sIds (consEmpleado cuil)

-- Costo: O(S logS) en cada instancia de la recursión sobre la lista de id sectores se
-- realiza una operación de costo logS -incorporarSector-
agregarSectores :: [SectorId]  -> Empleado -> Empleado
agregarSectores []         empl = empl
agregarSectores (sId:sIds) empl = incorporarSector sId (agregarSectores sIds empl)   

-- Costo: O(logS + E)
agregarEmplEnSectores :: Empleado -> [SectorId] -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
agregarEmplEnSectores empl []         sects = sects
agregarEmplEnSectores empl (sId:sIds) sects = 
    agregarEmplEnSector empl sId (agregarEmplEnSectores empl sIds sects)

-- Costo: O(logS + E)
agregarEmplEnSector :: Empleado -> SectorId -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
agregarEmplEnSector empl sId sects = 
    assocM sId (empleadosDelSectorActualizado empl sId sects) sects

-- Costo: O(logS + E) 
empleadosDelSectorActualizado :: Empleado -> SectorId -> Map SectorId (Set Empleado) -> Set Empleado
empleadosDelSectorActualizado empl sId sects = 
    (case (lookupM sId sects) of 
                (Just empleados) -> addS empl empleados
                Nothing          -> error "no existe el sector en la empresa") 




-- =================================================================

-- Propósito: agrega un sector al empleado con dicho CUIL.
-- Costo: O()
agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
agregarASector sId cuil (ConsE sects empls) = 
    let empl = buscarPorCUIL cuil (ConsE sects empls)
        in
        let empleadoActualizado = buscarPorCUIL cuil (ConsE sects (actualizarEmpleado sId empl empls))
            in ConsE (agregarEmplEnSector empleadoActualizado sId sects)
                     (actualizarEmpleado sId empl empls)



-- Costo: O(logE + S)
actualizarEmpleado :: SectorId -> Empleado -> Map CUIL Empleado -> Map CUIL Empleado
actualizarEmpleado sId empl empls = 
    assocM (cuil empl) (incorporarSector sId empl) empls 
 
-- =================================================================

-- Propósito: elimina al empleado que posee dicho CUIL.
-- Costo: O(log E + S logS), asumiendo que la op. buscarPorCUIL es de costo log E y,
-- removerEmplM es de costo S logS
borrarEmpleado :: CUIL -> Empresa -> Empresa
borrarEmpleado cuil (ConsE sects empls) = 
    let empleadoABorrar = buscarPorCUIL cuil (ConsE sects empls)
          in ConsE (removerEmplM empleadoABorrar (sectores empleadoABorrar) sects)
                   (deleteM cuil empls)

-- Costo: O(S logS + log E) recursión sobre la lista de sectores donde aparece el empleado a borrar, en donde en 
-- cada instancia se realiza assocM de costo logS y removerEmplS con costo log E.
removerEmplM :: Empleado -> [SectorId] -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
removerEmplM empl []         sects  = sects
removerEmplM empl (sId:sIds) sects  = assocM sId (removerEmplS empl (case (lookupM sId sects) of (Just empls) -> empls)) 
                                                 (removerEmplM empl sIds sects)

-- Costo: O(log E)
removerEmplS :: Empleado -> Set Empleado -> Set Empleado
removerEmplS empl empls = removeS empl empls