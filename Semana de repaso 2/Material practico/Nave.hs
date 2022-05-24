

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)
              --Sectores de nave     --Tripulante de nave    --Trip. ord segun rango

{- 
    INV. REP.: 
    * El segundo map y el MaxHeap tienen que tener los mismos tripulantes;
      eso implica que no solo tengan el mismo nombre sino tambien los mismos sectores asignados
      en la nave.
    * Ningun tripulante puede tener asignado un id de sector que no pertenezca a la nave, es decir, que
      no exista en el primer map.
    * Por cada sector que tenga asignado un tripulante, ya sea en el segundo Map o en la MaxHeap, 
      su nombre tiene que pertenecer en los tripulantes de dicho sector dentro del primer Map.
-}

-- Implementación

-- ===========================================
-- b)

--Costo: O(S log S) asumiendo que la función consSectores es de costo S log S
construir :: [SectorId] -> Nave
construir sIds = N (consSectores sIds) emptyM emptyH

-- Costo: O(S log S) siendo S el tamaño de la lista de identificadores de sectores, sobre la que se hace RE,
-- y en donde en cada instancia de la recursión se hace uso de assocM con costo log S.
consSectores :: [SectorId] -> Map SectorId Sector
consSectores []         = emptyM
consSectores (sId:sIds) = assocM sId (crearS sId) (consSectores sIds)


-- ===========================================
-- c) 

-- Costo: O(log T) asumiendo que las operaciones assocM e insertH son de costo log T.
ingresarT :: Nombre -> Rango -> Nave -> Nave
-- Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
ingresarT nom ran (N sectores tripM tripH) =
    let nuevoT = crearT nom ran  
      in N sectores (assocM nom nuevoT tripM) (insertH nuevoT tripH)
    

-- ===========================================
-- d) 

-- Costo: O(logM) también podría ser log T? ya que se hace lookupM sobre el map de tripulantes.
sectoresAsignados :: Nombre -> Nave -> Set SectorId
-- Propósito: Devuelve los sectores asignados a un tripulante.
-- Precondición: Existe un tripulante con dicho nombre.
sectoresAsignados nom (N _ tripM _) = 
  case (lookupM nom tripM) of
    (Just trip) -> sectoresT trip
    Nothing     -> error "No se cumple la precondicion"


-- ===========================================
-- e) 

-- Costo: O(log S) asumiendo que lookupM es de costo log S 
datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
-- Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
-- Precondición: Existe un sector con dicho id.
datosDeSector sId (N sectores _ _) = 
  case (lookupM sId sectores) of 
    (Just sector) -> (tripulantesS sector, componentesS sector) 
    Nothing       -> error "No se cumple la precondicion"


-- ===========================================
-- f) 

-- Eficiencia: O(log T) el costo a mi me queda T. ya que se asume que la op. setToList es de costo
-- T.
tripulantesN :: Nave -> [Tripulante]
-- Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
tripulantesN (Nave _ _ tripH) = setToList tripH
