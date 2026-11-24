
--Definicion de funciones 
--Factorial
fac_p 0 = 1
fac_p x = x * fac_p(x - 1)

--Ecuacion con guardas
fac_G1 (x)
    | x == 0 = 1
    | x >= 1 = x * fac_G1(x -1)

--variacion
fac_G2 (x)
    | x == 0 = 1
    | otherwise = x * fac_G2(x -1)

------------- Recursion----------------
--Recursion numerica ya trabajada en fac_p 
--Si usas _  le estás diciendo: "Agarra ese primer elemento, pero no te molestes en guardarlo con ningún nombre porque no me interesa saber qué es"
--Recursion sobre lista (mismo que length con import prelude)
tam :: [a] -> Int
tam [] = 0 
tam (_:xs) = 1 + tam xs
{-
Cuando haces una separación por patrón como (x:xs) o (_:xs), estás partiendo la lista en dos partes:
Lo que está antes de los dos puntos (_ o x): Es el primer elemento (la cabeza).
Lo que está después de los dos puntos (xs): Es la lista restante sin el primer elemento (la cola).
Por ejemplo, si tienes la lista [10, 20, 30]:
El primer elemento (_) es 10.
El resto de la lista (xs) es [20, 30].
-}
--Recursion sobre lista :Reversa
revertir :: [a] -> [a]
revertir [] = []
revertir (x:xs) = (revertir xs) ++ [x]
{-
revertir [2,4,6] = (revertir [4,6]) ++ [2]
= ((revertir [6]) ++ [4]) ++ [2]
= ((revertir [] ++ [6]) ++ [4]) ++ [2]
= (([] ++ [6]) ++ [4]) ++ [2]
= ([6] ++ [4]) ++ [2]
= [6,4] ++ [2]
= [6,4,2]
-}
--Recursion sobre varios argumentos
mizip :: [a] -> [b] -> [(a,b)]
mizip [] _ = []
mizip _ [] = []
mizip (x:xs) (y:ys) = (x,y) : zip xs ys

{-
zip [1,3,5] [2,4,6,8] = (1,2) : (zip [3,5] [4,6,8])
= (1,2) : ((3,4) : zip [5] [6,8])
= (1,2) : ((3,4) : ((5,6) : (zip [] [8])))
= (1,2) : ((3,4) : ((5,6) : [])))
= (1,2) : ((3,4) : [(5,6)])
= (1,2) : [(3,4),(5,6)]
= [(1,2),(3,4),(5,6)]
-}

--Recursion Mutua: Posicion pares e impares //Remarco da las POSICIONES no el valor
pares :: [a] -> [a]
pares [] = []
pares (x:xs) = x: impares xs

impares :: [a] -> [a]
impares [] = []
impares (_:xs) = pares xs
---------------------
milast :: [a] -> a
milast [x] = x
milast (x:xs) = milast xs 

miinit :: [a] -> [a]
miinit [x] = []
miinit (x:xs) = x : miinit xs

minull :: [a] -> Bool
minull [] = True --Si esta vacia devuelve true 
minull (_:xs) = False -- sino falso 

buscar :: Eq a=> a ->[a] -> Bool
buscar _[] = False
buscar a (x:xs) 
    | a == x =True --Si lo encuentro true listo
    | otherwise = buscar a xs  --Si no lo encuentro, sigo buscando en el resto de (xs)


mitake :: Int -> [a] -> [a]
mitake 0 _      = []    -- Si pides 0 elementos, devuelves lista vacía (no importa la lista `_`)
mitake _ []     = []    -- Si la lista está vacía, no hay nada que tomar
mitake n (x:xs) 
    | n > 0     = x : mitake (n - 1) xs  -- Si n es mayor a 0, guardas el elemento y pides n-1 al resto
    | otherwise = []


midrop :: Int -> [a] -> [a]
midrop 0 xs     = xs   -- Si pides descartar 0 elementos, te devuelves la lista tal como está
midrop _ []     = []   -- Si la lista está vacía, no hay nada que descartar
midrop n (_:xs) 
    | n > 0     = midrop (n - 1) xs  -- Si n > 0, descartas el actual (`_`) y restas 1 a n con el resto
    | otherwise = xs                 -- Si n es negativo o 0, devuelves la lista original
{-
//otras Importantes
- last :: [a] -> a   //Devuelve el ultimo elemento de una lista 
- init :: [a] -> [a]    //Devuelve todo los elementos exepto el ultimo
Ejemplo: init [1, 2, 3] devuelve [1, 2]
-null :: [a] -> Bool // Comprueba si una lista esta vacia 
Ejemplo: null [] devuelve True
-elem :: Eq a => a -> [a] -> Bool //Verifica si un elem pertenece a una lista.
Ejemplo: elem 3 [1, 2, 3, 4] devuelve True

-take :: Int -> [a] -> [a] --El valor de take n xs es la lista constituida por los primeros n elementos de la lista xs 
:Ejemplo   3 [5,4,3,2,1] entonces  [5,4,3]
-drop :: Int -> [a] -> [a] el de drop n xs es lo que quedas de la lista
Ejemplo : 3 [8,4,2,1,5,6] entonces [1,5,6]

-}