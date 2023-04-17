data ArtistaMusical = Cantante Registro TipoCanto Trayecto | Instrumentista Instrumento Trayecto deriving(Show)

data Registro = Soprano | Contralto | Tenor | Baritono | Bajo deriving(Show)
data TipoCanto = Solista | Coral deriving(Show)
data Instrumento = Violin | Clarinete | Trompeta | Timbales deriving(Show)
type Trayecto = Int 

--b)

trayectoria_musical :: ArtistaMusical -> Trayecto
trayectoria_musical (Cantante r tp trayecto) = trayecto
trayectoria_musical (Instrumentista i trayecto) = trayecto

--c)

instance Eq ArtistaMusical where
    (Cantante r1 tp1 trayecto1) == (Cantante r2 tp2 trayecto2) = trayecto1 == trayecto2 
instance Ord ArtistaMusical where
    (Cantante r1 tp1 trayecto1)<= (Cantante r2 tp2 trayecto2) = trayecto1 <= trayecto2 

{- ======================Ejercicio 2======================================== -}

--a)
instance Eq Registro where
    Soprano == Soprano = True
    Contralto == Contralto = True
    Tenor == Tenor = True
    Baritono == Baritono = True
    Bajo == Bajo = True
    _ == _ = False

buscarSolistas :: [ArtistaMusical] -> Registro -> [ArtistaMusical]
buscarSolistas [] _ = []
buscarSolistas (artista:as) r = case artista of
    Cantante reg Solista _ | reg == r -> artista : buscarSolistas as r
    _ -> buscarSolistas as r

--b)
{- [(Cantante Soprano Solista 1),(Instrumentista Violin 1),(Cantante Soprano Coral 1)] -}

--c) buscarSolistas [(Cantante Soprano Solista 1),(Instrumentista Violin 1),(Cantante Soprano Coral 1)] Soprano = [Cantante Soprano Solista 1]

{- ==================Ejercicio3======================== -}
data ListaAsoc a b = Vacia | Nodo a b ( ListaAsoc a b ) deriving (Show)
{- Aca me estaba diciendo que el segundo parametro tenia que ser del mismo tipo que b(en el Nodo) -}

la_algun_multiplo :: ( Integral b) => ListaAsoc a b -> b -> Bool
la_algun_multiplo Vacia _ = False
la_algun_multiplo (Nodo a b lista) divisor
  |  (b `mod` divisor == 0) = True
  | otherwise = la_algun_multiplo lista divisor