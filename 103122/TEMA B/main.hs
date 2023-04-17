{- Ejercicio 1 -}

--a)
data Color = Rojo | Azul | Blanco | Gris deriving Show


data EstiloAuto = Sedan | Coupe deriving Show

type Potencia = Int 
data Vehiculo = Moto Color Potencia | Auto Color EstiloAuto Potencia deriving Show

--b)
potencia_vehiculo :: Vehiculo -> Potencia
potencia_vehiculo (Moto color potencia) = potencia
potencia_vehiculo (Auto color est potencia) = potencia

--c) Incluir el tipo Vehiculo al la clase Ord de manera tal que se considere menor o igual segun su potencia. Ademas incluir el tipo Vehiculo a la clase Show
instance Eq Vehiculo where
    (Moto color1 potencia1) == (Moto color2 potencia2) = potencia1 == potencia2
    (Auto color1 est1 potencia1) == (Auto color2 est2 potencia2) = potencia1 == potencia2


instance Ord Vehiculo where
    (Moto color1 potencia1) <= (Moto color2 potencia2) = potencia1 <= potencia2
    (Auto color1 est1 potencia1) <= (Auto color2 est2 potencia2) = potencia1 <= potencia2

{- instance Show Vehiculo where
    show (Auto color1 est1 potencia1) = "|Auto| Color: " ++ show color1 ++ ", Estilo: "++ show est1 ++ ", Potencia: " ++ show potencia1
-}

{- Ejercicio 2 -}
--a) Programar de manera recursiva

pintarCoupes :: [Vehiculo] -> Color -> [Vehiculo]
pintarCoupes [] _ = []
pintarCoupes (vehiculo:vs) color = case vehiculo of 
    Auto _color Coupe p | True -> Auto color Coupe p : pintarCoupes vs color
    _ -> pintarCoupes vs color

--b) [(Auto Rojo Coupe 1),(Moto Azul 2),(Auto Blanco Coupe 3)]

--c) pintarCoupes [(Auto Rojo Coupe 1),(Moto Azul 2),(Auto Blanco Coupe 3)] Gris = [Auto Gris Coupe 1,Auto Gris Coupe 3]

{- Ejercicio 3 -}
data ListaAsoc a b = Vacia | Nodo a b ( ListaAsoc a b ) deriving (Show)

la_es_cota_inf :: (Eq a,Ord a) => ListaAsoc a b -> a -> Bool
la_es_cota_inf Vacia _ = True
la_es_cota_inf (Nodo a b _lista) clave = (clave <= a) && la_es_cota_inf _lista clave
                                        