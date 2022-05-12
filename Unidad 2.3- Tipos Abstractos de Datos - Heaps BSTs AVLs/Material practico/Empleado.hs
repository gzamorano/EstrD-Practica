
consEmpleado :: CUIL -> Empleado
--Propósito: construye un empleado con dicho CUIL.
--Costo: O(1)
cuil :: Empleado -> CUIL
--Propósito: indica el CUIL de un empleado.
--Costo: O(1)
incorporarSector :: SectorId -> Empleado -> Empleado
--Propósito: incorpora un sector al conjunto de sectores en los que trabaja un empleado.
--Costo: O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.
sectores :: Empleado -> SectorId
--Propósito: indica los sectores en los que el empleado trabaja.
--Costo: O(1)