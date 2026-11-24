f::Float->Float
f x = 3*x + 2  

sumDig1::Int->Int
sumDig1 0 = 0 
sumDig1 x = (mod x 10 ) + sumDig1 (div x 10)

sumDig2::Int->Int
sumDig2 x 
    |x<=9 = x
    |otherwise = (mod x 10 ) + sumDig2 (div x 10)

---Funciones como Parametro
doble :: Int -> Int 
doble x = 2*x

aplicaFuncion:: (Int->Int) -> Int -> Int
aplicaFuncion f x = f x

--aplicaFuncion doble 4 == 8

-----Composicion de funciones 
inc :: Int -> Int
inc = (+1)

dec :: Int -> Int
dec x = x - 1

componer :: (Int -> Int) -> (Int -> Int) -> Int -> Int
componer f g = f.g

--Parcializacion 
sumaP :: Int -> (Int -> Int)
sumaP x y = x + y
{-
sumaP toma un valor entero x y devuelve una funcion sumaP
x que toma un entero y y devuelve la suma de x e y
-}
multP :: Int -> (Int -> (Int -> Int))
multP x y z = x * y * z
{-
mult toma un entero x y devuelve la funcion mult x que
toma un entero y devuelve la funcion mult x y que toma un
entero z y devuelve x * y * z.
-}
