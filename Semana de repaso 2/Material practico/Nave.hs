

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)
              --Sectores de nave     --Tripulante de nave    --Trip. ord segun rango

{- 
    INV. REP.: en N ms mt h
    * ms y mt tienen los mismos tripulantes.
    * Ningun tripulante puede tener asignado un SectorId que no sea una clave en ms.
    * Para todo SectorId que tenga asignado un tripulante en mt, su nombre pertenece
      a los tripulantes de dicho sector en ms.
    * Para toda asociacion id -> s en ms, sectorId s == id.
    * Para toda asociacion n -> t en mt, nombre t == n.  
-}

{-
los de clase: 
en N ms mt h
- mt y h tiene los mismos elementos.
- para toda asociacion n -> t, para todo id E sectores t, para el s asociado a id en ms, vale 
  n E tripulantes s.
- para toda asociacion id -> s, para todo n E tripulantes s, sea n -> t, vale id e sectores t.
- para toda asociacion id -> s, sectorId s == id.
- para toda asociacion n -> t, nombre t == n.
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

-- Costo: O(log M) también podría ser log T? ya que se hace lookupM sobre el map de tripulantes.
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

-- Costo: O(T log T) asumiendo que tripOrdenados es de costo T log T.
tripulantesN :: Nave -> [Tripulante]
-- Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
tripulantesN (Nave _ _ tripH) = tripOrdenados tripH

-- Costo: O(T log T) por cada tripulante de la heap, lo agrego a la lista y lo borro, 
-- el borrado tiene el mayor costo, que es log T.
tripOrdenados :: MaxHeap Tripulante -> [Tripulante]
tripOrdenados tripH = if (isEmptyH tripH)
                        then []
                        else maxH tripH : tripOrdenados (deleteMaxH tripH)

-- ===========================================
-- g) 

-- Costo: O(C + log S), siendo C la cantidad de componentes dados.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
-- Propósito: Asigna una lista de componentes a un sector de la nave.
agregarASector cs sId (N sectores tripM tripH) = N (agregarASectorM cs sId sectores) tripM tripH


-- Costo: O(C + log S) asumiendo que la op. agregarASectorS es de costo lineal, lookupM y assocM
-- tienen costo log S.
agregarASectorM :: [Componente] -> SectorId -> Map SectorId Sector -> Map SectorId Sector
agregarASectorM cs sId sectores = 
  case (lookupM sId sectores) of 
    (Just sector) -> assocM sId (agregarASectorS cs sector) sectores
    Nothing       -> error "No existe sector"


-- Costo: O(C) asumiendo que agregarC es de costo constante y se realiza tal operación en cada
-- instancia de la recursión sobre la lista de componentes.
agregarASectorS :: [Componente] -> Sector -> Sector
agregarASectorS []     s = s
agregarASectorS (c:cs) s = agregarC c (agregarASectorS cs s)


-- ===========================================
-- h)

-- Costo: O(log S + T log T) asumiendo que: asignarASectorS tiene costo logS + logT
--                                          asignarASectorTM tiene costo logT + logS
--                                          asignarASectorTH tiene costo T log T
asignarASector :: Nombre -> SectorId -> Nave -> Nave
-- Propósito: Asigna un sector a un tripulante.
-- Nota: No importa si el tripulante ya tiene asignado dicho sector.
-- Precondición: El tripulante y el sector existen.
asignarASector nom sId (N sectores tripM tripH) = 
  N (asignarASectorS nom sId sectores) 
    (asignarASectorTM nom sId tripM) 
    (asignarASectorTH nom sId tripH)


-- Costo: O(log S + log T) asumiendo que lookupM y assocM son de costo log S y agregarT de costo log T.
asignarASectorS :: Nombre -> SectorId -> Map SectorId Sector -> Map SectorId Sector
-- PRECOND: El sector existe en el map
asignarASectorS nom sId sectores = case (lookupM sId sectores) of
                                      (Just sector) -> assocM sId (agregarT nom sector) sectores
                                      Nothing       -> error "No se cumple la precondicion"

-- Costo: O(log T + log S) asumiendo que lookupM y assocM son de costo log T y asginarS costo log S.
asignarASectorTM :: Nombre -> SectorId -> Map Nombre Tripulante -> Map Nombre Tripulante
-- PRECOND: El tripulante existe en el map
asignarASectorTM nom sId tripM = case (lookupM nom tripM) of
                                  (Just trip) -> assocM nom (asignarS sId trip) tripM
                                  Nothing     -> error "No se cumple la precondicion"

-- Costo: O(T log T) en cada instancia de la recursión sobre el map de tripulantes se realiza
-- la op. deleteMaxH de costo log T.
asignarASectorTH :: Nombre -> SectorId -> MaxHeap Tripulante -> MaxHeap Tripulante
-- PRECOND: Existe en la heap un tripulante asociado al nombre dado 
asignarASectorTH nom sId tripH = 
  if (isEmptyH tripH)
    then emptyH
    else if (nom == nombre (maxH tripH))
            then insertH (asignarS sId (maxH tripH)) (deleteMaxH tripH)
            else asignarASectorTH nom sId (deleteMaxH tripH)


-- =======================================================
-- Implementación de funciones como usuario del tipo Nave.
-- =======================================================

-- ===========================================
-- i)


-- Costo: O(T S log S) asumiendo que losNoVacios tiene costo T S log S. 
sectores :: Nave -> Set SectorId
-- Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados).
sectores nave = losNoVacios (tripulantesN nave)

-- Costo: O(T S log S) en cada instancia de la recursión sobre la lista de tripulantes, se realiza la op. 
-- unionS con costo S log S.
losNoVacios :: [Tripulante] -> Set SectorId
losNoVacios []     = emptyS
losNoVacios (t:ts) = unionS (sectoresT t) (losNoVacios ts) 


-- ===========================================
-- j)

-- Costo: (T) asumiendo que T es la cantidad de tripulantes de la nave y que 
-- tripulantesSinSectores tiene costo T. 
sinSectoresAsignados :: Nave -> [Tripulante]
-- Propósito: Devuelve los tripulantes que no poseen sectores asignados.
sinSectoresAsignados nave = tripulantesSinSectores (tripulantesN nave)

-- Costo: O(T) en cada instancia de la recursión sobre la lista de tripulantes, se realizan las op. 
-- noTieneSectores y cons (:), ambas de costo constante.
tripulantesSinSectores :: [Tripulante] -> [Tripulante]
tripulantesSinSectores []     = []
tripulantesSinSectores (t:ts) = if (noTieneSectores t)
                                  then t : tripulantesSinSectores ts
                                  else tripulantesSinSectores ts

-- Costo: O(1) asumiendo que sizeS y sectoresT son de costo constante.
noTieneSectores :: Tripulante -> Bool
noTieneSectores t = (sizeS (sectoresT t)) == 0

-- ===========================================
-- k)

-- Costo: (S^3 + s) asumiendo que barrilesS tiene costo S^3 y s es el costo de la función sectores(T S logS). 
barriles :: Nave -> [Barril]
-- Propósito: Devuelve todos los barriles de los sectores asignados de la nave.
barriles nave = barrilesS (sectores nave) nave

-- Costo: O(S^3) asumiendo que barillesDeSectores tiene costo S^3 
barrilesS :: Set SectorId -> Nave -> [Barril]
barrilesS ss nave = barrilesDeSectores (setToList ss) nave 

-- Costo: O(S^3) en cada instancia de la RE sobre identificadores de sectores se realiza la op. 
-- barrilesDelSectorCs con costo cuadrático.
barrilesDeSectores :: [SectorId] -> Nave -> [Barril]
barrilesDeSectores []         nave = []
barrilesDeSectores (sId:sIds) nave = 
  let (_, cs) = datosDeSector sId nave
    in barrilesDelSectorCs cs ++ barrilesDeSectores sIds nave 


-- Costo: O(C^2) siendo C el tamaño de la lista, sobre la que se hace RE, donde en cada instancia de 
-- la misma se realiza la op. append de costo lineal.
barrilesDelSectorCs :: [Componente] -> [Barril]
barrilesDelSectorCs []     = []
barrilesDelSectorCs (c:cs) = barrilesC c ++ (barrilesDelSectorCs cs) 

-- Costo: O(1)
barrilesC :: Componente -> [Barril]
barrilesC Almacen bs = bs
barrilesC _          = []


-- BONUS
-- l) Posible representación para el tipo Sector.

data Sector = ConsS SectorId [Componente] (Set Nombre)
