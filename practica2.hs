import Data.Char (isAlpha, toUpper)
--Ejercicio 3 del parcial 2025 
{-Se desea implementar un sistema de cifrado por palabra en Haskell 
El cifrado sustituye cada letra del mensaje segun un alfabeto cifrado constudo a partir de una palabra clave
Procedimiento:
-Toma la palabra clave y elimina letras repetidas
-Construir el alfabetp cifrado : primero las letras de la clave ,luego las letras restantes dek alkafabeto ingles  en orden alfabetico
-Cada letra del mensaje se remplaza por la letra correspondiente en el alfabeto cifrado (misma posicion)
-los caracteres que no son letras (espacion , signos ) se mantienen igual
EJ: 
Clave : "HASHKELL" procesada : "HASHKEL"
alfabeto cifrado : "HASKELBCDFGIJMNOPQRTUVWXYZ"
Mensaje: "HOLA MUNDO" -> cifrado : "CNIH JUMKN"
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
cifrar mensaje clave = cifrarAux mensaje alfabetoDeseado
  where
    alfabetoOriginal = ['A'..'Z']
    alfabetoDeseado  = alfabetoCifrado clave
    
    -- Función auxiliar para no recalcular el alfabeto en cada paso
    cifrarAux [] _ = []
    cifrarAux (a:as) alfcif
        | not (isAlpha a) = a : cifrarAux as alfcif  -- Si es espacio o símbolo, se queda igual
        | otherwise       = let pos = posicion (toUpper a) alfabetoOriginal
                            in posElem pos alfcif : cifrarAux as alfcif



--busca un elemento y retorna la posicion
posicion :: Eq a=> a -> [a] -> Int
posicion _ [] = -1
posicion ele (x:xs)
    | ele == x = 0
    | otherwise = 1 + posicion ele xs

--retorna un elemento en una posicion
posElem :: Eq a=> Int -> [a] -> a
posElem 0 (x:_) = x
posElem n (x:xs) = posElem (n-1) xs  

--desifrar :: String -> String -> String 

--Practica Modelo 2024 TEMA B

cifradoVigenere :: String -> String -> String 
cifradoVigenere []_=[]
cifradoVigenere xs ys = cifradoVigenereAux xs ys 0

cifradoVigenereAux :: String -> String -> Int -> String
cifradoVigenereAux [] _ _ = []
cifradoVigenereAux _ [] _ = []
cifradoVigenereAux (x:xs) (y:ys) i
    | buscar x alf = [  posElem xs (posicion x alf + posicion y alf)  ] : cifradoVigenereAux xs ys (i+1)
    | otherwise = x : cifradoVigenereAux xs ys i

    where
        alf = ['a'..'z']
        


devolverPosicion :: String -> Int -> Char
devolverPosicion (x:xs) n 
    | n == 0 = x
    | otherwise = devolverPosicion xs (n-1)