-- Ejercicio 1

-- max2 :: (Float, Float) -> Float
-- max2 (x, y) | x >= y = x
--             | otherwise = y
-- NO currificada
-- currificada Float -> Float -> Float

-- normaVectorial :: (Float, Float) -> Float
-- normaVectorial (x, y) = sqrt (x^2 + y^2)
-- NO currificada
-- currificada Float -> Float -> Float

-- subtract :: Float -> Float -> Float (se le da un float y devuelve una funcion)
-- subtract = flip (-)

-- predecesor :: Float -> Float
-- predecesor = subtract 1

-- evaludarEnCero :: (Float -> a) -> a
-- evaluarEnCero = \f -> f 0

-- dosVeces :: (a -> a) -> a -> a
-- dosVeces = \f -> f . f

-- se aplica la funcion flip a cada elemento de la lista los cuales son funciones
-- flip es una funcion que toma dos argumentos y devuelve una funcion con los argumentos intercambiados
-- flipAll :: [a -> b -> c] -> [b -> a -> c]
-- flipAll = map flip

-- flipRaro ::
-- flipRaro = flip flip

-- Ejercicio 2

-- La clave esta en la definicion de tipos, con eso se puede deducir el comportamiento de las funciones
curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y) -- dada una funcion (no currficada) y dos argumentos, x e y, devuelve f de la tupla porque f no es currificada, el tipo de curry luego hace que la currifique

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

-- Ejercicio 3

-- reduce una lista desde la derecha usando una función y un valor inicial.reduce una lista desde la derecha usando una función y un valor inicial.
-- los argumentos de la funcion son un elemento de la lista y el resultado acumulado hasta ese punto
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f z [] = z
-- foldr f z (x:xs) = f x (foldr f z xs)

sumFoldr :: (Num a) => [a] -> a
sumFoldr = foldr (+) 0

elemFoldr :: (Eq a) => a -> [a] -> Bool
elemFoldr elem = foldr (\x resultadoRestoLista -> x == elem || resultadoRestoLista) False

ppFoldr :: [a] -> [a] -> [a]
-- ppFoldr = foldr (:)
-- ok pero si los parametros son xs ys concatenaria ys xs en vez de xs ys como hace (++) asi se arregla:
ppFoldr = flip (foldr (:))

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr predicado = foldr (\x res -> if predicado x then x : res else res) []

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\x res -> f x : res) []

-- Solo para listas no vacías. no requiere un valor inicial.
-- Toma el último elemento de la lista como valor inicial y aplica la función de derecha a izquierda.
-- foldr1 :: (a -> a -> a) -> [a] -> a
-- foldr1 _ []     = error "foldr1: empty list"
-- foldr1 _ [x]    = x
-- foldr1 f (x:xs) = f x (foldr1 f xs)

mejorSegún :: (a -> a -> Bool) -> [a] -> a
mejorSegún f = foldr1 (\x y -> if f x y then x else y)

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- (acumulador -> elemento -> nuevo acumulador)
-- foldl _ z []     = z
-- foldl f z (x:xs) = foldl f (f z x) xs

sumasParciales :: (Num a) => [a] -> [a]
sumasParciales = foldl (\res x -> (x + (if length res >= 1 then head res else 0)) : res) []

sumaAlternada :: (Num a) => [a] -> a
-- esto alcanza porque la forma en que foldr anida las restas produce ese patrón de + y –.
sumaAlternada = foldr (-) 0

sumaAlternadaDesdeDerecha :: (Num a) => [a] -> a
sumaAlternadaDesdeDerecha = sumaAlternada . reverse

-- Ejercicio 4

-- concatMap aplica una función a cada elemento de una lista y concatena los resultados
-- concatMap :: (a -> [b]) -> [a] -> [b]
-- xs !! i (elemento en pos i de lista xs) (y en este caso)
-- let <definiciones> in <expresión>
permutaciones :: [a] -> [[a]]
permutaciones xs =
  concatMap
    ( \i ->
        let (y, ys) = (xs !! i, take i xs ++ drop (i + 1) xs)
         in map (y :) (permutaciones ys)
    )
    [0 .. length xs - 1]

-- Ejercicio 5

-- elementosEnPosicionesPares no es usa recursion estructural porque hace usa xs sin g
-- entrelazar no es usa recursion estructural porque tanto xs como ys si se usan debe ser con g

-- Ejercicio 6

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarUna :: (Eq a) => a -> [a] -> [a]
sacarUna elem = recr (\x xs res -> if x == elem then xs else x : res) []

insertarOrdenado :: (Ord a) => a -> [a] -> [a]
insertarOrdenado elem = recr (\x xs res -> if x > elem then elem : x : xs else x : res) [elem]

-- Ejercicio 7

-- map :: (a -> b) -> [a] -> [b]
-- map _ []     = []
-- map f (x:xs) = f x : map f xs

-- toma funcion currficada de dos args y una lista de pares de valores y devuelve lista de aplicaciones de la funcion en cada par
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
-- mapPares f = map (\x -> f (fst x) (snd x))
mapPares f = map (Prelude.uncurry f)

-- (zip)
armarPares :: [a] -> [b] -> [(a, b)]
-- armarPares _ [] = []
-- armarPares [] _ = []
-- armarPares (x:xs) (y:ys) = (x, y) : armarPares xs ys
-- armarPares _ _ = []
armarPares = foldr (\x res ys -> if null ys then [] else (x, head ys) : res (tail ys)) (const [])

-- con foldr evito recursion explicita
-- las funciones currificadas pueden aplicarse parcialmente, es decir, pasarles menos argumentos
-- de los que esperan y obtener una nueva función esperando los argumentos restantes.

-- (zipWith)
mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f = foldr (\x res ys -> f x (head ys) : res (tail ys)) (const [])

-- res es el resultado parcial de foldr que espera una lista

-- Ejercicio 9

-- foldNat (+1) 0 3 suma 3 veces 1 al valor inicial
foldNat :: (a -> a) -> a -> Integer -> a
foldNat f z 0 = z
foldNat f z n = f (foldNat f z (n - 1))

potencia :: Integer -> Integer -> Integer
potencia n = foldNat (*n) 1-- p

-- Ejercicio 10

genLista :: a -> (a -> a) -> Integer -> [a]
genLista x f 0 = []
-- genLista x f n = x : genLista (f x) f (n-1)
genLista x f n = foldNat (\rec -> rec ++ [f (last rec)]) [x] (n-1)

desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta x y | y > x = genLista x (+1) (y-x+1)

-- Ejercicio 12

data AB a = Nil | Bin (AB a) a (AB a)

foldAB :: (b -> a -> b -> b) -> b -> AB a -> b
foldAB _ z Nil = z
foldAB f z (Bin i r d) = f (foldAB f z i) r (foldAB f z d)

recrAB :: (AB a -> b -> a -> AB a -> b -> b) -> b -> AB a -> b
-- explicacion de la funcion: arbol izq, res de recursion izq, raiz, arbol der, res de recu der, devuelve b
recrAB _ z Nil = z
recrAB f z (Bin i r d) = f i (recrAB f z i) r d (recrAB f z d)

esNil :: AB a -> Bool
esNil Nil = True
esNil _   = False

-- esNil :: AB a -> Bool
-- esNil arbol = case arbol of
--   Nil -> True
--   Bin _ _ _ -> False

altura :: AB a -> Integer
altura = foldAB (\recI raiz recD -> 1 + max recI recD) 0

cantNodos :: AB a -> Integer
cantNodos = foldAB (\recI raiz recD -> 1 + recI + recD) 0

-- Ejercicio 13

ramas :: AB a -> [[a]]
ramas = foldAB (\recI raiz recD -> case (recI, recD) of 
                                  ([], []) -> [[raiz]]
                                  _ -> map (raiz:) recI ++ map (raiz:) recD ) []

cantHojas :: AB a -> Integer
cantHojas = foldAB (\recI _ recD -> case (recI, recD) of 
                                      (0, 0) -> 1
                                      _ -> recI + recD ) 0

espejo :: AB a -> AB a
-- espejo Nil = Nil
-- espejo (Bin i r d) = Bin (espejo d) r (espejo i)
espejo = foldAB (\recI raiz recD -> Bin recD raiz recI) Nil

-- Ejercicio 15

data RoseTree r = Rose r [RoseTree r]

foldRose :: (r -> [b] -> b) -> RoseTree r -> b
foldRose f (Rose r rs) = f r (map rec rs)
                        where rec = foldRose f
    
ramasR :: RoseTree a -> [[a]]
ramasR = foldRose (\x rec -> if null rec 
                            then [[x]]
                            else map (x:) (concat rec))
    
-- Ejercicio 17
-- [1, 3]

-- Ejercicio 18
paresDeNat :: [(Int,Int)]
pares = [(x,y-x) | y <- [0..], x <- [0..y-1]]

-- Ejercicios 20 21

listasQueSuman :: Int -> [[Int]]
listasQueSuman 0 = [[]]
listasQueSuman n | n > 0 = [x : xs | x <- [1..n], xs <- listasQueSuman (n-x)]

listaConListas :: [[Int]]
listaConListas = [xs | n <- [1..], xs <- listasQueSuman n]

-- >>> 
