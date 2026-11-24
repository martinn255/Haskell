doble x = x + x 
-- :t (como no esta def doble me muestra doble :: Num a => a -> a)

suma :: Int->Int->Int 
-- Si pongo en la terminal :t (me muetra) suma :: Int -> Int ->Int (tipo con lo que def)
suma x y = x + y

norma :: Float -> Float -> Float 
--Al asignarle Float me lo achica la presicion, Si no declaraba nada por defecto me tiraba double
norma x y = sqrt(x^2 + y^2);

doble2 x = 2 * x

funcConstante :: Int -> Int 
-- Cualquier valor que le asignes se vuele 1 
funcConstante x = 1

--Decidir si un numero es positivo
esPositivo :: Int -> Bool
esPositivo x = x == abs(x) 

--Una funcion que comprueba si su argumento esta entre cero y nueve
--Nota Mia: Me llama la atencion argumento
entreNumANum :: Int -> Bool
entreNumANum x = x<=9 && x>=0

-- Definir la funcion esMultiploDeTres que devuelva True si es un numero es multiple de 3
esMultiploDeTres :: Int -> Bool
esMultiploDeTres x = x `mod`3 == 0

-- Definir la funcion celsius2Fahr que pasa una temperatura en grados Cerlsius a grados Fahrenheit
celsius2Fahr :: Float -> Float
celsius2Fahr c = c * 18 + 32


{-
//EL TIPO BOOL
Bool = ({True, False}, {&&,||,not})
Funciones y Operadores
-(&&) :: Bool -> Bool -> Bool  //Conjuncion logica.
-(||) :: Bool -> Bool -> Bool  //Disyuncion logica.
- (not) :: Bool -> Bool  //Negacion logica.
- (otherwise) :: Bool  //Funcion constante que devuelve elvalor True.

//EL TIPO INT e INTEGER
Funciones y Operadores:
- (+), (-), (*) :: Int -> Int -> Int //Suma, resta y producto de enteros.
- (∧) :: Int -> Int -> Int  //Operador potencia; elexponente debe ser natural.
-  div, mod :: Int -> Int -> Int //Cociente y resto.
-  abs :: Int -> Int Valor //absoluto.
-  even, odd :: Int -> Bool //Par o impar de un numero.

//EL TIPO FLOAT Y DOUBLE
Funciones y Operadores:
- (+), (*), (-), (/) :: Float -> Float -> Float  //Suma, producto, resta y division real.
- (∧) :: Float -> Int -> Float //Potencia de base real,pero exponente natural.
- (**) :: Float -> Float -> Float //Potencia de base y exponente real.
- sqrt :: Float -> Float //Raız cuadrada.
- pi :: Float //El valor del numero π.
- truncate, round, floor, ceiling :: Float ->
Integer/Int.
- fromInt :: Int -> Float y fromInteger :: Integer-> Float //funciones de conversion de tipo.

//EL TIPO CHAR
Un valor constante de tipo caracter se escribre entre comillas simples.
- ’\n’  //es el caracter de salto de lınea.
- ’\t’ //es el caracter tabulador.
- ’\” //es el caracter comilla.
- ’\”’ //es el caracter comilla doble.
Funciones:
- ord :: Char -> Int //Devuelve el codigo ASCIIcorrespondiente al caracter argumento.
//Ej : ord 'A' devuelve 65
- chr :: Int -> Char //Funcion inversa a ord.
//Ej : chr 65 devuelve 'A'
- isUpper :: Char ->Bool. //verifica si el caracter es una letra mayus
- isLower :: Char ->Bool. //verifica si el caracter es una minuscula
- isDigit :: Char ->Bool. //verifica si el caracter es un num del('0' al '9')
- isAlpha :: Char ->Bool. //verifica si el caracter es una letra del alfabeto(Mayus o Minus ,sin contar num ni simbolos)

- toUpper :: Char -> Char. //Convierte cualquier minus a mayus (simbolo y num no cambia)
- toLower :: Char -> Char. //Convierte cualquier mayus a minus ()

-}
