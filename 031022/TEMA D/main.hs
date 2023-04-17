data Deportista = Futbolista Nombre Zona Titulos 
                | Tenista Nombre Categoria Titulos 
                | Velocista Nombre Titulos 
type Nombre = String
type Titulos = Int
data Zona = Arco | Defensa | Mediocampo | Delantera
data Categoria = Simples | Dobles

--b)
misma_zona :: Zona -> Zona -> Bool
misma_zona Arco Arco = True
misma_zona Defensa Defensa = True
misma_zona Mediocampo Mediocampo = True
misma_zona Delantera Delantera = True
misma_zona _ _ = False


--c)
puntaje_titulos :: Deportista -> Int
puntaje_titulos d = case d of
    Tenista _ _ t -> t
    Futbolista _ _ t -> t
    Velocista _ t -> t

--d) Incluir el tipo Deportista en la clase Ord
instance Ord Deportista where
    d1 > d2 = puntaje_titulos(d1) > puntaje_titulos(d2)

