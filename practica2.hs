import Data.Char (isAlpha, toUpper)
import Prelude hiding(map,filter)
import Data.Char (toUpper)
import Data.List (elemIndex)

--Ejercicio 3 del parcial 2025 
{-Se desea implementar un sistema de cifrado por palabra en Haskell 
El cifrado sustituye cada letra del mensaje segun un alfabeto cifrado constudo a partir de una palabra clave
Procedimiento:
-Toma la palabra clave y elimina letras repetidas
-Construir el alfabeto cifrado : primero las letras de la clave ,luego las letras restantes del alfabeto ingles  en orden alfabetico
-Cada letra del mensaje se remplaza por la letra correspondiente en el alfabeto cifrado (misma posicion)
-los caracteres que no son letras (espacion , signos ) se mantienen igual
EJ: 
Clave : "HASHKELL" procesada : "HASHKEL"
alfabeto cifrado : "HASKELBCDFGIJMNOPQRTUVWXYZ"
Mensaje: "HOLA MUNDO" -> cifrado : "CNIH JUMKN"
lo que arriba quiere hacer es Si tengo ABCDEFGHIJKLMNOPQRSTWXYZ  y el cifrado 
Entonces HOLA MUNDO ahora H en el alfabeto tiene la pos 8 y en el cifrado en la pos 8 esta el C asi se
-}

procesarClave :: String -> String
procesarClave [] = []
procesarClave (x:xs) = x: procesarClave[ y | y <- xs , y /= x ]

alfabetoCifrado :: String -> String 
alfabetoCifrado [] = []
alfabetoCifrado xs = clavelimpia ++  [a | a <- ['A'..'Z'] , not( buscar a clavelimpia)] -- nota directamente se pone (y `elem` clavelimpia)
    where
        clavelimpia = procesarClave xs

buscar :: Eq a=> a -> [a] -> Bool
buscar _ [] = False
buscar a(x:xs)
    | x == a = True 
    | otherwise = buscar a xs

cifrar :: String -> String -> String
cifrar [] _ = []
cifrar (m:mens) clave = cifrarAux m alfOrig alfNuevo : cifrar mens clave  
    where
        alfOrig = ['A'..'Z']
        alfNuevo = alfabetoCifrado clave

cifrarAux :: Char -> String -> String -> Char
cifrarAux c as bs = case elemIndex (toUpper c) as of --elemIndex (toUpper c) as evalúa si toUpper c aparece dentro de la lista as
    Just i  -> posElem i bs   -- si le encuentra devuelve la pos i y busco ahora el nuevo elemento i en el nuevo alfabeto bs y retorno char 
    Nothing -> c        -- si no es letra en 'A'..'Z' (espacio, signo, etc.), queda igual

desifrar :: String -> String -> String 
desifrar [] _ = []
desifrar (m:mens) clave = cifrarAux m alfNuevo alfOrig : desifrar mens clave
    where 
        alfOrig = ['A'..'Z']
        alfNuevo = alfabetoCifrado clave


--busca un elemento y retorna la posicion
posicion :: Eq a=> a -> [a] -> Int
posicion _ [] = -1
posicion ele (x:xs)
    | ele == x = 0
    | otherwise = 1 + posicion ele xs

--retorna un elemento en una posicion 
posElem :: Int -> [a] -> a
posElem 0 (x:_)  = x
posElem n (_:xs)
  | n > 0     = posElem (n - 1) xs
  | otherwise = error "Indice negativo"
posElem _ []     = error "Indice fuera de rango"




-------Ejercicion Parcial 2018------------
--aplicando filter y map y composicion de funciones de Haskell , definir las sig funciones 
--Ej : Promedio [[8,6] ,[7,9,4],[6,2,4],[9,6]] => [7,6.67 , 4 , 7.5]
mimap :: (a-> b) -> [a]->[b]
mimap _[] = []
mimap f (x:xs) = f x : mimap f xs

promedios :: [[Integer]] -> [Double]
promedios xs = mimap promLista xs 

promLista :: [Integer] -> Double
promLista [] = 0
promLista xs = fromIntegral(sum xs) / fromIntegral (milength xs)


milength :: [Integer] -> Integer 
milength [] = 0
milength (x:xs) = (1 + milength xs )

---Parte del eje 4 ----
-- que hcae g = (*)(-1) ----> lo que hace es multiplicar por -1 cualquier valor osea g :: Num a=> a-> a
-- oSea g 4 devuelve -4

{-
f e l = map (g . (e+)) ( filter (5 <) l )
f :: (Num a, Ord a) => a -> [a] -> [a] 
1_lo que haces es filtra la lista l : filtra (5<) l conserva solamente valores mayores a 5 
2_Compone suma y cambio de signo: (g . (e+)) aplica primero (e+ x) (suma e al elemento) 
y luego g (multiplica el resultado por -1), es decir, calcula -(e + x) o (-1) * (e + x).
3_Transforma los elementos filtrados: map aplica esa función compuesta a cada elemento que pasó el filtro y devuelve la nueva lista resultante
EJEJMPLO:f 10 [2, 6, 8, 3]:Mayores que 5: [6, 8].Sumar 10 y multiplicar por -1: [-(10 + 6), -(10 + 8)] $\rightarrow$ [-16, -18].
-}
{-
Ejercicio: Programación Funcional: 
La criptografía es el conjunto de estrategias que permiten transformar la información para protegerla de accesos no autorizados. 
Un algoritmo conocido es el Cifrado por Transposición, que consiste en reorganizar las letras de un mensaje en una tabla con un 
número determinado de columnas. Al leer las letras de las columnas en lugar de las filas, se obtiene un mensaje cifrado. Por ejemplo, 
el mensaje “Haskell es genial” se puede organizar en una tabla de 4 columnas, obteniendo el mensaje cifrado: “Heeelalsn sl i k ga ”.

Implemente en Haskell las siguientes funciones:

CodificacionTransposicion :: String -> Int -> String, donde el primer parámetro es el mensaje a cifrar y el segundo parámetro 
indica el número de columnas en las que se debe organizar el texto.

DecodificacionTransposicion :: String -> Int -> String, donde el primer parámetro es el mensaje cifrado y el segundo parámetro 
indica el número de columnas utilizadas para realizar la transposición original.

tema A ->
-}
