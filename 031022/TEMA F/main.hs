{- Ejercicio 1 -}
data Color = Rojo | Verde| Azul | Negro | Blanco 

-- Programar la funcion usando pattern matching

mismoColor :: Color -> Color -> Bool
mismoColor Rojo Rojo = True
mismoColor Verde Verde = True
mismoColor Azul Azul = True
mismoColor Negro Negro = True
mismoColor Blanco Blanco = True
mismoColor _ _ = False

--b)
{- Sinonimos de tipo -}
type Nombre = String
type Costo = Int
type Dano = Int
type Resistencia = Int
{- Definicion de Tipos -}
data CartaMagic = CartaDeTerreno Nombre Color | CartaDeCriatura Nombre Costo Dano Resistencia deriving Eq


--c)

cuantoDano :: CartaMagic -> Int
cuantoDano (CartaDeTerreno _ _) = 0
cuantoDano (CartaDeCriatura _ _ dano _) = dano
--d)
instance Eq Color where
    Rojo == Rojo = True
    Verde == Verde = True
    Azul == Azul = True
    Negro == Negro = True
    Blanco == Blanco = True
    _ == _ = False
instance Ord CartaMagic where
    (CartaDeCriatura n1 c1 dano1 r1) > (CartaDeCriatura n2 c2 dano2 r2) = cuantoDano(CartaDeCriatura n1 c1 dano1 r1) > cuantoDano(CartaDeCriatura n2 c2 dano2 r2)

{- Ejercicio 2 -}

soloTerreno :: [CartaMagic] -> Color -> [Nombre]
soloTerreno [] _ = []
soloTerreno (carta:ns) c = case carta of
    CartaDeTerreno nombre color | color == c -> nombre : soloTerreno ns c
    _ -> soloTerreno ns c 

--b)[(CartaDeTerreno "Lobo Piola" Rojo) ,(CartaDeTerreno "Alaskan" Rojo),(CartaDeCriatura "Oso Pardo" 100 17 89)]

--c) soloTerreno [(CartaDeTerreno "Lobo Piola" Rojo) ,(CartaDeTerreno "Alaskan" Rojo),(CartaDeCriatura "Oso Pardo" 100 17 89)] Rojo = ["Lobo Piola","Alaskan"]

{- Ejercicio 3 -}
data ListaAsoc a b = Vacia | Nodo a b ( ListaAsoc a b ) deriving (Show)

la_menores :: (Ord b) => ListaAsoc a b -> b -> ListaAsoc a b
la_menores Vacia _ = Vacia
la_menores (Nodo a b lista) x | x > b = Nodo a b (la_menores lista x)
                            |otherwise = la_menores lista x