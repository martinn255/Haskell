import Prelude hiding(map,filter)

duplicar :: [Int] -> [Int]
duplicar [] = []
duplicar (x:xs) = (2*x) : duplicar xs

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = (x+1) : sucesores xs

cuadrados :: [Float] -> [Float]
cuadrados []=[]
cuadrados (x:xs) = (x*x)  : cuadrados xs

aplicar :: (Int -> Int) -> Int -> Int
aplicar f x = f x

{-
map :: (a -> b) -> [a] -> [b]
Aplica una función dada a cada uno de los elementos de una lista y devuelve una nueva lista con los resultados.

Ejemplo: map (*2) [1, 2, 3] multiplica cada número por 2 y devuelve [2, 4, 6].
-}

mimap  :: (a -> b) -> [a] -> [b]
mimap _[] = []  -- Si la lista está vacía, no hay nada que transformar
mimap f(x:xs) = f x : mimap f xs    -- Al primer elemento (x) le aplicamos la función (f), y seguimos con el resto (xs)

duplicar1 :: [Int] -> [Int]
duplicar1 x = mimap (2*) x 
    -- :t (2*) :: num a => a -> a

sucesores1 :: [Int] -> [Int]
sucesores1 x = mimap(+1) x

cuadrados1 :: [Float] -> [Float]
cuadrados1 x = mimap (^2) x

{-
filter :: (a -> Bool) -> [a] -> [a]
Filtra una lista conservando únicamente los elementos que cumplen una condición (un predicado que devuelve True).

Ejemplo: filter even [1, 2, 3, 4, 5, 6] filtra solo los números pares y devuelve [2, 4, 6].
-}
mifilter :: (a -> Bool) -> [a] ->[a]
mifilter _ [] = []  -- Si la lista está vacía, el resultado es vacío
mifilter f (x:xs)       -- `p` es la función condición (ej. es par, es mayor que 5, etc.
    | f x       = x  : mifilter f xs    -- Si se cumple la condición para x, lo conservamos y seguimos
    | otherwise = mifilter f xs         -- Si no se cumple, descartamos x y seguimos con el resto

{-
where es una herramienta muy útil para definir variables, funciones auxiliares o constantes de manera local justo al final de una función o de una guarda.
Te permite limpiar el código: en lugar de escribir expresiones largas o repetir cálculos dentro de tu función, los calculas una sola vez dándoles un nombre claro en el bloque where.
-}
--Ejemplo
raizCuadratica :: Double -> Double -> Double -> (Double, Double)
raizCuadratica a b c = 
    ((-b + sqrt discriminante) / (2 * a), 
     (-b - sqrt discriminante) / (2 * a))
  where
    -- Definimos la variable local 'discriminante' aquí abajo
    discriminante = b^2 - 4 * a * c

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [ y | y <- xs , y <= x] ++ [x] ++ qsort[ y | y <- xs , y > x]