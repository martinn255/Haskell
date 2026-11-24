--Trabajo Practico 2

-----------Ejercicio1 -Funciones basicas-----------

--a)Dado un numero, devuelve si es menor que cero
esNumeroNegativo :: Double -> Bool
esNumeroNegativo x = x < 0 
--b)Dado dos numeros , devuelve si el primero es multiplo del segundo
esMultDe :: Int -> Int -> Bool
esMultDe x y = (mod x y) == 0
--c)Dado tres numeros , devuelve si el primero esta fuera del rango qeu forma los dos restantes
fueraDeRango :: Int -> Int -> Int -> Bool
fueraDeRango x y z = (x>=y) && (x<=z)

-----------Ejercicio 2 -Tuplas----------------
--a)Devuelve el´punto medio de dos puntos p1 y p1
ptoMedio :: (Float,Float) -> (Float,Float) -> (Float,Float) 
ptoMedio (x1,y1)(x2,y2) = ( (x1 + x2) / 2 ,(y1 +y2) / 2)
--b)Norma eucleida
norma :: (Float,Float) -> Float
norma (x,y) = sqrt(x^2 + y^2)
--c) Dada una cantidad de segundos, retorna la cantidad de hs,min,seg
segTiempo :: Integer -> (Integer,Integer,Integer)
segTiempo x = (hs,min,seg)
    where
        hs = (div x 3600)
        min = (div (mod x 3600) 60 )
        seg = (mod (mod x 3600) 60)

-----------Ejercicio 3 Funciones sobre listas----------------

--A)
--Elim todas las apariciones de cualquier caracter de la primera cadena en la seg
--EJ: limpiar “susto” “puerta” == “pera”
limpiar :: String -> String -> String
limpiar _ [] = []
limpiar x(y:ys)
    | busca y x = limpiar x ys 
    | otherwise = y : limpiar x ys
--Busca  EJ :2 en [1,2,3] retorna Bool
busca :: Eq a => a -> [a] -> Bool
busca _ [] = False
busca a (x:xs)
    | a == x = True
    | otherwise = busca a xs

--B)
--Devuelve la diferencia de cada uno con el promedio general
dif :: [Float] -> [Float]
dif [] = []
dif xs = [x - prom | x <- xs] --Verificar porque los resultados tambien da en negativo y el abs solo funciona en int 
    where 
        prom = promedio xs
--Promedio float         
promedio :: [Float] -> Float
promedio [] = 0
promedio xs =  suma xs / tam xs
--Mi suma de todo
suma :: [Float] -> Float
suma [] = 0
suma (x:xs) = x + suma xs
--Mi tam
tam :: [a] -> Float
tam [] = 0
tam (x:xs) = 1 + tam xs 

--C)
-- Indica si una lista de enteros tiene todos sus elementos iguales
todosIguales :: [Int] -> Bool
todosIguales [] = True --lista vacia
todosIguales [_] = True --lista de un solo elemento
todosIguales (x:y:xs) --Significa que tomo 2 elem de la lista 
    | x == y = todosIguales (y:xs) -- el segundo elemento que tome lo devuelvo a la lista
    | otherwise = False

--D)
--Repetir cada eleemnto de la lista n veces
repLista :: [Int] -> Int -> [Int]
repLista [] _ = []
repLista (x:xs) n = repElem x n ++ repLista xs n
--Repite ejemplo el 2 3 veces ASi [2,2,2]
repElem :: Int -> Int -> [Int]
repElem _ 0 = []
repElem x n = x : repElem x (n-1)

--E)
--Dada una lista de listas de enteros , devolver una unica lista con los elem de todas las listas
lista2lista :: [[Int]] -> [Int]
lista2lista xss = [x | xs <- xss , x <- xs]
--Ej : [[1, 2], [3], [4, 5, 6], []]

--F)
--Devuelve verd si la cant de parentsis '(' es la misma cant de  ')'
checkParentesis :: String -> Bool
checkParentesis [] = True
checkParentesis xs = cantParIzq xs == cantParDer xs  
 
--Cuenta la cant de parentesis izq y der 
cantParIzq :: String -> Int
cantParIzq [] = 0
cantParIzq (x:xs)
    | x == '(' = 1 + cantParIzq xs
    | otherwise = cantParIzq xs
cantParDer :: String -> Int
cantParDer [] = 0
cantParDer (x:xs)
    | x == ')' = 1 + cantParDer xs
    | otherwise = cantParDer xs 

--G)
--Devuelve los  N elementos Finales de una lista 
--ej 3 [1,2,3,4,5] devuelve [3,4,5]

finales :: Integer -> [Integer] -> [Integer]
finales _ [] = []
finales n (x:xs)  
    | n <=0 = []
    | n>=len = xs
    | otherwise = drop(fromIntegral(len -n)) xs --drop elimina los primeros len -n ej 5-3 elimina los primeros 2
    where
        len = fromIntegral(length xs) 

---------Ejercicio 4------------
--b)
--Replica 
replica :: Int -> a -> [a]
replica n x 
    | n <= 0    = []
    | otherwise =  x : replica (n-1) x