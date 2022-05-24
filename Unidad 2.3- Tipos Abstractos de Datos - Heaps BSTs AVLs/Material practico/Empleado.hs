module Empleado 
    (Empleado, consEmpleado, cuil, incorporarSector, sectores)
where 

import Set1

type CUIL = Int
type SectorId = Int

data Empleado = Empl CUIL 
                     (Set SectorId) 



consEmpleado :: CUIL -> Empleado
--Propósito: construye un empleado con dicho CUIL.
--Costo: O(1)
cuil :: Empleado -> CUIL
--Propósito: indica el CUIL de un empleado.
--Costo: O(1)
incorporarSector :: SectorId -> Empleado -> Empleado
--Propósito: incorpora un sector al conjunto de sectores en los que trabaja un empleado.
--Costo: O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.
sectores :: Empleado -> [SectorId]
--Propósito: indica los sectores en los que el empleado trabaja.
--Costo: O(1)

consEmpleado cuil = Empl cuil emptyS

cuil (Empl cuil _) = cuil

incorporarSector sId (Empl cuil sectores) = Empl cuil (addS sId sectores)

sectores (Empl _ sectores) = setToList sectores

yo = incorporarSector 1
    $ incorporarSector 5
    $ consEmpleado 20431078741


instance Show Empleado where
    show e = "Empl <" ++ show (cuil e) ++ " " ++ show (sectores e) ++ ">"


instance Eq Empleado where
    e1 == e2 = cuil e1 == cuil e2