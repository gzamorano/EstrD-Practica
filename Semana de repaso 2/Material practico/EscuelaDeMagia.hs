type Nombre = String
type Hechizo = String
data EscuelaDeMagia = EDM (Set Hechizo) (Map Nombre Mago) (PriorityQueue Mago)

{-
    INV.REP: en EDM s m pq
        * m y pq poseen los mismos elementos.
        * todos los hechizos de cada mago están en s.
        * en pq no existen dos magos con el mismo nombre.
        * el mago en el map para un nombre tiene ese nombre
-}

--b)
fundarEscuela :: EscuelaDeMagia
fundarEscuela = EDM emptyS emptyM emptyPQ
-- Costo: O(1) asumiendo que emptyS, emptyM y emptyPQ son operaciones de costo constante, 
-- por la implementación dada

--c)
estaVacia :: EscuelaDeMagia -> Bool
estaVacia (EDM _ _ pq) = isEmptyPQ pq
-- Costo: O(1) asumiendo isEmptyPQ de costo constante, por la implementación de PQ dada.

--d)
registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
registrar nom (EDM s m pq) = 
    case (lookupM nom m) of
        (Just mago) -> EDM s m pq
        Nothing     -> let nuevoMago = crearM nom
                        in EDM s (assocM nom nuevoMago m) (insertPQ nuevoMago pq)
-- Costo: O(log M) asumiendo que lookupM, assocM e insertPQ son de costo log M.

--e)
magos :: EscuelaDeMagia -> [Nombre]
magos (EDM _ m _) = domM m
-- Costo: O(M) asumiendo que domM tiene costo lineal.

--f)
hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
-- PRECOND: existe un mago con dicho nombre en la escuela
hechizosDe nom (EDM _ m _) = 
    case (lookupM nom m) of
        (Just mago) -> hechizos mago
        Nothing     -> error "No se cumple la precondición" 
-- Costo: O(log M) asumiendo que lookupM tiene costo log M, según la implementacion de map dada, y
-- y, las operaciones case y hechizos tiene costo constante.

--g)
leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
-- PRECOND: existe un mago con dicho nombre en la escuela
leFaltanAprender nom (EDM s m _) =
    case (lookupM nom m) of
        (Just mago) -> sizeS s - (sizeS (hechizos m))
        Nothing     -> error "No se cumple la precondicion"
-- Costo: O(log M) asumiendo que lookupM es de costo log M, hechizos de costo de costo constante y
-- sizeS de costo constante sobre el set de hechizos, pero los costos constantes se desprecian y
-- me quedo con log M.

--h)
egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
-- PRECOND: hay al menos un mago en la escuela
egresarUno (EDM s m pq) = 
    let mago = findMaxPQ
        in (mago, EDM s (deleteM (nombre mago) m) (deleteMaxPQ pq))
-- Costo: O(log M) asumiendo que findMaxPQ y nombre son de costo constante, deleteM y deleteMaxPQ de costo log M. 

--i)
enseñar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
-- PRECOND: existe un mago con dicho nombre en la escuela
enseñar h nom (EDM s m pq) =
    case (lookupM nom m) of
        (Just mago) -> let nuevoMago = aprender h mago
                        in EDM (addS h s) (assocM nom nuevoMago m) (modificarPQ nuevoMago pq)
        Nothing     -> error "No se cumple la precondición"
-- Costo: O(M log M + log H) asumiendo que addS tiene costo log H, assocM costo log M y modificarPQ
-- tiene costo (M log M + log H), donde este último absorbe los costos de las primeras operaciones mencionadas.

--PROPÓSITO: reemplaza el mago de la PQ que sea igual al dado, por éste último
modificarPQ :: Mago -> PriorityQueue Mago -> PriorityQueue Mago
-- PRECOND: hay un mago con el mismo nombre en la PQ que el mago dado
modificarPQ m pq = if (m == findMaxPQ pq)
                    then insertPQ m (deleteMaxPQ pq)
                    else insertPQ (findMaxPQ pq) (modificarPQ m (deleteMaxPQ pq))
-- Costo: O(M log M) en peor caso (cuando se busca el mago con menos prioriodad), se hace recursión sobre el 
-- tamaño de la cola de magos y en cada instancia de la recursión de magos, se 
-- usan findMaxPQ de costo constante, insertPQ y deleteMaxPQ con costo log M.


-- USUARIO 

--j)
hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo
hechizosAprendidos escuela = todosLosHechizos (magos escuela) escuela
-- Costo: O(M * (log M + H log H)) asumiendo que 'todosLosHechizos' tiene tal costo y la operación magos
-- tiene costo M, el cual es absorbido por el M del costo en la primera operacion mencionada.

todosLosHechizos :: [Nombre] -> EscuelaDeMagia -> Set Hechizo
todosLosHechizos [] escuela = emptyS
todosLosHechizos (n:ns) escuela = unionS (hechizosDe n escuela) (todosLosHechizos ns escuela)
-- Costo: O(M * (log M + H log H)) se hace recursión estructural sobre la lista de nombres de magos, 
-- y en cada instancia de la misma se usa 'hechizosDe' con costo log M sobre el map de magos y 
-- 'unionS' de costo H log H sobre el set de hechizos.


--k) 
hayUnExperto :: EscuelaDeMagia -> Bool
hayUnExperto escuela = not (estaVacia escuela) && hayUnExpertoEn escuela
-- Costo: O(log M) asumiendo que hayUnExpertoEn es de costo log M y estaVacia es de orden constante, por lo
-- que se desprecia.

hayUnExpertoEn :: EscuelaDeMagia -> Bool
-- PRECOND: la escuela no está vacía
hayUnExpertoEn escuela = let mago = fst (egresarUno escuela)
                            in leFaltanAprender (nombre mago) escuela == 0
-- Costo: O(log M) asumiendo que egresarUno y leFaltanAprender son de costo log M y la operacion nombre
-- es de orden constante, por lo que se desprecia.


--i) 
egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
egresarExpertos escuela = if (hayUnExperto escuela)
                            then let (magoEgresado, escuelaSinMago) = egresarUno escuela
                                    in agregarEgresado magoEgresado (egresarExpertos escuelaSinMago)
                            else ([], escuela)
-- Costo: O(M log M) asumiendo que hayUnExperto y egresarUno son operaciones de costo log M y las mismas
-- se usan en cada instancia de la recursión sobre la cantidad de magos de la escuela.

agregarEgresado :: Mago -> ([Mago], EscuelaDeMagia) -> ([Mago], EscuelaDeMagia)
agregarEgresado m (ms, escuela) = (m:ms, escuela)
-- Costo: (1) asumiendo que la operacion cons es de orden constante.


--ALTERNATIVA como lo hace Fidel en el 8vo video de teoría. Se ahorra una subtarea

egresarExpertos' :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
egresarExpertos' escuela = 
    if (hayUnExperto escuela)
        then let (magoEgresado, escuelaSinMago)     = egresarUno escuela
                 (magosEgresados, escuelasSinMagos) = egresarExpertos' escuelaSinMago
              in (magoEgresado:magosEgresados, escuelaSinMagos) 
        else ([], escuela)

























