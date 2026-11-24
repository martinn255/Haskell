
import Data.Char (toLower)
--Ejercicio 1
--Determinar si un anio es bisiesto(Ej: 2000==True,1900==False,2024==True)
esBisiesto :: Int -> Bool
esBisiesto x = ((mod x 4) ==0 && (mod x 100) /= 0) || (mod x 400) == 0   
--Otra Forma
bisi :: Int ->Bool
bisi x 
    | (mod x 400) == 0 = True
    | (mod x 100) == 0 = False
    | (mod x 4)   == 0 = True 
    | otherwise        = False

--Ejercicio 2
--Contar Pares
esPar :: Int->Bool
esPar x=(x `mod` 2) ==0 

contarPares:: [Int]->Int
contarPares []=0
contarPares (x:xs)
    | esPar x = 1 + contarPares(xs)
    | otherwise = contarPares(xs)

--solo pares 
soloPares :: [Int]->[Int]
soloPares[] = []
soloPares (x:xs) 
    | esPar x = x :soloPares (xs)
    | otherwise = soloPares (xs)

--Ejercicio 3 
-- Producto de los elementos de una lista(Ej:[1,2,3,4]==24)
productoLista :: [Int] -> Int
productoLista [] =1
productoLista (x:xs) = x * productoLista xs

--Ejercicio 4
--Generar lista de divisores de un numero(Ej: 6==[1,2,3,6])
divisores :: Int -> [Int]
divisores x = [ y | y <- [1..x] , (mod x y) == 0 ] --y es el elemento que voy a agregar a al lista


--Ejercicio 5
--Verifica si un numero es primo(Ej: 7==True, 4 ==False)
esPrimo :: Int -> Bool
esPrimo n = divisores n == [1, n]

--Ejercicio 6 
--Generar lista de numeros primos hasta n (Ej: 10 == [2,3,5,7])
primosHasta :: Int -> [Int]
primosHasta n = [ x | x <-[1..n], esPrimo x ]

--Ejercicio 7
--suma de los cuadrados de una lista(Ej:[1,2,3]==14,[]==0)
sumaCuadrados :: [Int] -> Int 
sumaCuadrados [] = 0
sumaCuadrados(x:xs) = (x^2 + sumaCuadrados xs)

--Ejercicio 8
--Contar Cuantas vocales tiene un string(Ej:"Hola" ==2, "xyz"==0 )
inc :: Int -> Int 
inc x = x + 1

contarVocales :: String -> Int
contarVocales [] = 0
contarVocales (x:xs)
    | busca x ['a','e','i','o','u'] =  1 + contarVocales xs
    | otherwise = contarVocales xs

--Ejercicio 9 
--Eliminar elementos repetidos de una lista(Ej:[1,2,2,3,1,4]==[3,4,2,1] ,"programacion" == "gramcionp")
sinRepetidos :: Eq a=> [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs)
    | busca x xs = sinRepetidos xs --busco el x en la lista xs si lo encuentro sigo en recursion sin incluir a x
    | otherwise = sinRepetidos xs ++ [x] --Si el x no vuele a aparecer en la lista lo proceso y lo agrego al final

--Otra forma (Esta crea una lista nueva)
sinRepetidos2 :: Eq a=> [a] -> [a]
sinRepetidos2 []=[]
sinRepetidos2 (x:xs) = x : sinRepetidos2[ y | y <- xs, y /= x]

--Ejercicio 10
--Suma los digitos de todos los numeros en una lista  
-- Pruebas [12,34] == 10  --> (1+2)+(3+4) yyy  [409,7] == 23 --> (4+0+9)+(7)
sumaDig :: Int ->Int
sumaDig 0=0
sumaDig x = (mod x 10) + sumaDig (div x 10)

sumaDigLista :: [Int] -> Int
sumaDigLista [] =0
sumaDigLista (x:xs) = (sumaDig x) + sumaDigLista (xs)


---------------Ejercicios varios (parte 1)
--Muy importante busca un elemento en una lista 
busca :: Eq a => a -> [a] -> Bool
busca _ [] = False
busca a (x:xs)
    | a == x = True
    | otherwise = busca a xs 

--Ejercicio 1 
--Retorna una tupla con incremento pasado por parametro 
incTupla :: (Int,Int,Int) -> Int -> (Int,Int,Int)
incTupla (x,y,z)n = (x+n , y+n , z+n)

--Ejercicio 2
--Eliminar todas las apariciones de cualquiern caracter de la primera cadena en la segunda
--EJ: limpiar “susto” “puerta” == “pera”
limpiar :: String -> String -> String
limpiar _ [] = []
limpiar xs (y:ys)
    | busca y xs = limpiar xs ys  --Si encuentra un repetido no lo agrega sigue
    | otherwise = y : limpiar xs ys --caso contrario no lo encuentra lo agrega al nuevo y

--Ejercicio 3 
--3) Retorna la lista normalizada (resta con el promedio)
--Ej: (10,20,30)/3 = 20 depues a [10-20,20-20,30-20] esto es 
difPromedio :: [Float] -> [Float]
difPromedio []=[]
difPromedio xs= [ x - prom | x <- xs]
    where 
        cant =lengthFl xs
        prom = sumaLista xs / cant

sumaLista :: [Float] -> Float
sumaLista [] = 0
sumaLista (x:xs) = (x + sumaLista xs)

lengthFl :: [Float] -> Float
lengthFl [] = 0
lengthFl (x:xs) = 1 + lengthFl xs

--Ejercicio 4 
--Retorna True si todos los elementos son iguales
--Ej [1,1,,1,1,1] = True
todosIguales :: [Integer] -> Bool
todosIguales [] = True
todosIguales [_] = True
todosIguales (x:y:xs)  
    |(x==y) = todosIguales (y:xs) --Hace  1==1 y depues devuele el ultimo 1 a la lista y vuelve a comparar 
    | otherwise = False

--Ejercicio 5 Pares [1,2,3]  y [4,5,6,7,8] = [(1,4),(2,5),(3,6)]
pares :: [a] -> [b] -> [(a,b)]
pares [] _ = []
pares _ [] = []
pares (x:xs) (y:ys) = (x,y) : pares xs ys
--Terminado Ejercicion Varios Parte 1


-------------------Ejercicio Vario (Parte 2)----------------------------
--Ejercicio 1 --Es casi lo mismo que el buscar
pertenece :: Eq a=> a -> [a] -> Bool
pertenece _ [] = False
pertenece a (x:xs)
    | a == x = True
    | otherwise = pertenece a xs

--Ejercicio 2
--Interseccion ose que pertenesca a las 2 lista 
interseccion :: Eq a=>[a] -> [a] -> [a]
interseccion [][]=[]
interseccion xs ys = [ x | x <- xs , busca x ys]  --El busca o el pertence (misma funcion)

--Otra forma Recursion
inte :: Eq a=> [a] -> [a] -> [a]
inte [] _ =[]
inte (x:xs) ys
    | busca x ys =  x : inte xs ys
    | otherwise = inte xs ys 

--Ejercicio 3 
sinRepe :: Eq a=> [a] -> [a]
sinRepe [] = []
sinRepe (x:xs) 
    | not(busca x xs) = x : sinRepe xs 
    | otherwise = sinRepe xs
    
--Ejercicio 4 
union :: Eq a=> [a] -> [a] -> [a]
union xs ys = xs ++ [y | y <- ys , not(busca y xs)]

--Ejercicio 5 
--digitos2Enteros :: [Integer] -> Integer ---Lo que yo entiendo es que segun los dig formar un entero
dig2Entero :: [Integer] -> Integer
dig2Entero [] = 0
dig2Entero (xs) = armar 0 xs        --inicializo la suma =0
    where
        armar suma [] = suma 
        armar suma (y:ys) = armar (suma*10 + y) ys --con (y) siendo el dig  y (algo) siendo la suma 


--6 La criptografía es el conjunto de estrategias que permiten transformar la información para ocultarla a terceros no autorizados. Un algoritmo muy conocido es el “Código Murcielago”
--MURCIELAGO
--0123456789

--Una forma( De hacerlo) 
-- Función auxiliar para traducir un solo carácter según el código murciélago
traducirChar :: Char -> Char
traducirChar c = case lookup (toLower c) [('m','0'), ('u','1'), ('r','2'), ('c','3'), ('i','4'),
                                         ('e','5'), ('l','6'), ('a','7'), ('g','8'), ('o','9')] of
    Just num -> num      -- Si lo encuentra en la tabla, devuelve el número correspondiente
    Nothing  -> c        -- Si no está (espacios, otras letras), deja el carácter original

--Otra forma de cifrar la frase
codiMur2 :: String -> String 
codiMur2 [] = []
codiMur2 (x:xs) = traducirChar x : codiMur2 xs

-- Función principal que cifra la frase completa
codigoMurcielago :: String -> String
codigoMurcielago = mimap traducirChar

mimap  :: (a -> b) -> [a] -> [b]
mimap _[] = []  -- Si la lista está vacía, no hay nada que transformar
mimap f(x:xs) = f x : mimap f xs    -- Al primer elemento (x) le aplicamos la función (f), y seguimos con el resto (xs)

mifilter :: (a -> Bool) -> [a] ->[a]
mifilter _ [] = []  -- Si la lista está vacía, el resultado es vacío
mifilter f (x:xs)       -- `p` es la función condición (ej. es par, es mayor que 5, etc.
    | f x       = x  : mifilter f xs    -- Si se cumple la condición para x, lo conservamos y seguimos
    | otherwise = mifilter f xs         -- Si no se cumple, descartamos x y seguimos con el resto


--------------- Clases aparte para otras cosas
--Retornar Posicion (busco un elemento y retorna sus posiscion) 
posicion :: Eq a=> a -> [a] -> Int
posicion _ [] = -1
posicion ele (x:xs)
    | ele == x = 0
    | otherwise = 1 + posicion ele xs

--Sin sacar el elemento  de la lista 
posicionDe :: Eq a => a -> [a] -> Int
posicionDe elemento lista = buscar elemento lista 0
  where
    buscar _ [] _ = -1                 --Si llegamos al final y no está, devolvemos -1
    buscar e (x:xs) i
        | e == x    = i                --Si el elemento coincide, devolvemos la posición actual 'i'
        | otherwise = buscar e xs (i + 1) --Si no coincide, seguimos buscando y sumamos 1 a la posición

