{- Ejercicio 1 -}

--a) el tipo dedicacion no debe estar en la clase Eq
data Dedicacion = Simple | Semi | Full | Investigador deriving Show

{- Pattern matching -}
misma_dedicacion :: Dedicacion -> Dedicacion -> Bool
misma_dedicacion Simple Simple = True
misma_dedicacion Semi Semi = True
misma_dedicacion Full Full = True
misma_dedicacion Investigador Investigador = True
misma_dedicacion _ _ = False 

--b)
type Cantidad = Int
horas_trabajo :: Dedicacion -> Cantidad
horas_trabajo d = case d of
    Simple -> 10
    Semi -> 20
    Full -> 50
    Investigador -> 60
    
--c)
type Ingreso = Int
data Carrera = Matematica | Fisica | Computacion | Astronomia deriving (Show, Eq)
data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving (Eq,Show)
data Area = Administrativa | Ensenanza | Economica | Postgrado deriving (Eq,Show)

data Persona = Decano Dedicacion
            | Docente Cargo Dedicacion
            | NoDocente Area Dedicacion
            | Estudiante Carrera Ingreso deriving (Show)

{- Instancia para Dedicacion  porque sino no anda ord -}
{- instance Eq Dedicacion where
  Simple == Simple = True
  Semi == Semi = True
  Full == Full = True
  Investigador == Investigador = True
  _ == _ = False -}

instance Ord Dedicacion where
    d1 <= d2 = misma_dedicacion d1 d2 ||  horas_trabajo(d1) < horas_trabajo(d2)

{- Ejercicio 2 -}
{- solo_dedicacion :: [Persona] -> Dedicacion -> [Persona]
solo_dedicacion [] _ = []
solo_dedicacion (p:ps) r = case p of 
    Decano dedicacion | dedicacion == r -> p:solo_dedicacion ps r
    Docente _ dedicacion | dedicacion == r -> p:solo_dedicacion ps r
    NoDocente _ dedicacion |dedicacion == r -> p: solo_dedicacion ps r
    _ -> solo_dedicacion ps r -}

--b)[(Decano Simple),(Estudiante Matematica 1),(Docente Titular Simple)]

--c)
--solo_dedicacion [(Decano Simple),(Estudiante Matematica 1),(Docente Titular Simple)] Simple = [Decano Simple,Docente Titular Simple]



