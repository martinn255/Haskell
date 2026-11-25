----------------Ejercicio tema B----------
--tamanio
tam::[a]->Double
tam [] = 0
tam (x:xs) = 1 + tam xs
--Deriva
derivar::[Double]->[Double]
derivar [] = []
derivar [x] = []
derivar (x:xs) = (x * tam xs) : derivar xs

--Evalua una funcion ej f(4)= (4) + 3 
evaluar::[Double]->Double->Double
evaluar [] _ = 0
evaluar (x:xs) y = x * (y ** tam (xs)) + evaluar xs y
--Realiza newton 
newton::[Double]->Double->Double->Double
newton x y e
 |(abs (evaluar x y)) < e = y
 |otherwise = newton x (y - (evaluar x y)/(evaluar (derivar x) y)) e

---------------Ejercicio tema A -------------------
getPos::[(Double,Double)]->Int->Double
getPos [] _ = 0
getPos ((x,_):_) 0 = x
getPos (x:xs) i = getPos xs (i-1)

baseLagrange::[(Double,Double)]->Int->Double->Double
baseLagrange [] _ _ = 0
baseLagrange [x] _ _ = 1
baseLagrange ((x,y):(xs,ys):xss) 0 z = (z - xs)/(x - xs) * baseLagrange ((x,y):xss) 0 z
baseLagrange ((x,y):xs) i z = (z - x)/(getPos xs (i-1) - x) * baseLagrange xs (i-1) z

interpolar::[(Double,Double)]->Double->Double
interpolar [] _ = 0
interpolar x y = interpolarAux x x y

interpolarAux::[(Double,Double)]->[(Double,Double)]->Double->Double
interpolarAux _ [] _ = 0
interpolarAux original ((x,y):xs) z = y * (baseLagrange original ((length original) - (length ((x,y):xs))) z) + interpolarAux original xs z

-------De nuevo realizo 2026 B----
evaluarN :: [Double] -> Double -> Double 
evaluarN [] _ = 0
evaluarN (x:xs) n = x * (n**lengthD xs ) + evaluar xs n

lengthD :: [Double] -> Double
lengthD [] = 0
lengthD (x:xs) = (1 + lengthD xs)

derivarN :: [Double] -> [Double]
derivarN [] = []
derivarN (x:xs) =  x * lengthD (xs) : derivar xs 
     
newtonR :: [Double] -> Double -> Double -> Double
newtonR xs y e 
    | abs (evaluarN xs y) < e = y
    | otherwise = newtonR xs (y - (evaluarN (xs) y) / (evaluarN (derivar (xs)) y)) e 

 