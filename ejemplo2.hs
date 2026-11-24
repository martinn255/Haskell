f::Float->Float
f x = 3*x + 2  

--------------------------Listas

--listar :: Int -> Int -> Int -> [Int] que toma 3 elementos y los convierte en una lista.

lista :: Int -> Int -> Int ->[Int]
lista x y z = [x,y,z]  --Mejor Forma
--Otra forma 

lista1 :: Int -> Int -> Int -> [Int]
lista1 x y z = x : y : z : []

--------------------String ("hola" = ['h','o','l','a'] = 'h' : 'o' : 'l' : 'a' : [])
-- El String es renombre de [Char]

--Listas por Compresion 
--Definiciones por comprension en Matematicas:{x^2: x ∈ {2, 3, 4, 5}} = {4, 9, 16, 25}
--Cuadrados 
cuadrados :: Num a =>[a]->[a]
cuadrados xs =[ x ^ 2 | x <- xs]

--Concatencacion de lista de listas 
concatenar :: [[a]] -> [a]
concatenar xss = [x | xs <- xss, x <- xs] 
--Ejemplo [[1, 2], [3], [4, 5, 6], []] Devuelve [1, 2, 3, 4, 5, 6]

mihead :: [a] -> a
mihead (x:xs) = x

mitail :: [a] -> [a]
mitail (_:xs) = xs
--Nota como no uso x puedo poner _ directante



{-
algunas operaciones
- head :: [a] -> a  //devuelve el primer elemento de una lista
Ejemplo: head [1, 2, 3] devuelve 1
- tail :: [a] -> [a]    //devuelve una lista con todos los elementos excepto el primero 
Ejemplo: tail [1, 2, 3] devuelve [2, 3]
- (:) :: a -> [a] -> [a]    //Es el operador constructor de una lista.Agrega un elemento al princiopio de una lista existente
Ejemplo: 1 : [2, 3] devuelve [1, 2, 3]. (Es exactamente lo que usas en los patrones (x:xs))
- (++) :: [a] -> [a] -> [a]     //Concatena o une dos listas para formar una sola
Ejemplo: [1, 2] ++ [3, 4] devuelve [1, 2, 3, 4]
- length :: [a] -> Int  //Devuelve la cant de eleementos de una lista 
- reverse :: [a] -> [a]     //invierte el elemento de una lista
--------------------------------------------------
-}
