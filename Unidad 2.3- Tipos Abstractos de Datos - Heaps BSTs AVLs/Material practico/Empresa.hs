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
    INV.REP.: en ConsE (Map s (Set emp)) (Map c emp')
    *  ambos Map no poseen elementos repetidos
-}


{-
los empleados son un tipo abstracto.
el primer map relaciona id de sectores con los empleados que trabajan en dicho sector.
el segundo map relaciona empleados con su número de CUIL.
un empleado puede estar asignado a más de un sector
tanto Map como Set exponen una interfaz eciente con costos logarítmicos para inserción,
búsqueda y borrado, tal cual vimos en clase.
-}



-- Propósito: construye una empresa vacía.
-- Costo: O(1)
consEmpresa :: Empresa
consEmpresa = ConsE (emptyM) (emptyM)


-- Propósito: devuelve el empleado con dicho CUIL.
-- Costo: O(log E) siendo E la cantidad de empleados de la empresa.
buscarPorCUIL :: CUIL -> Empresa -> Empleado
buscarPorCUIL c (ConsE _ es) = lookupM c es



