-- null :: [a] -> Bool Devuelve True si la lista está vacía. null []
-- head :: [a] -> a
-- tail :: [a] -> [a]
-- init :: [a] -> [a] Devuelve la lista sin el último elemento
-- last :: [a] -> a
-- take :: Int -> [a] -> [a]
-- drop :: Int -> [a] -> [a] Elimina los primeros n elementos de una lista
-- (++) :: [a] -> [a] -> [a] . [1,2] ++ [3,4] == [1,2,3,4]
-- concat :: [[a]] -> [a]
-- reverse :: [a] -> [a]
-- elem :: Eq a => a -> [a] -> Bool

valorAbsoluto :: Float -> Float
valorAbsoluto x = if x < 0 then -x else x

bisiesto :: Int -> Bool
bisiesto x = x `mod` 4 == 0

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- inverso :: Float → Maybe Float . dado un número devuelve su inverso multiplicativo si está definido, o Nothing en caso contrario.

aEntero :: Either Int Bool -> Int -- convierte a entero una expresión que puede ser booleana o entera
aEntero (Left x) = x
aEntero (Right True) = 1
aEntero (Right False) = 0

limpiar :: String -> String -> String -- elimina todas las apariciones de cualquier carácter de la primera cadena en la segunda
limpiar _ [] = []
limpiar xs (y : ys) = if elem y xs then limpiar xs ys else y : limpiar xs ys

difCon :: [Float] -> Float -> [Float]
difCon [] _ = []
difCon (x:xs) n = (x-n) : difCon xs n       

promedio :: [Float] -> Float
promedio xs = sum xs / fromIntegral (length xs)

difPromedio :: [Float] -> [Float] -- devuelve la diferencia de cada item con el promedio general
difPromedio [] = []
difPromedio xs = difCon xs (promedio xs)

difPromedioCool :: [Float] -> [Float]
difPromedioCool [] = []
difPromedioCool xs = let p = promedio xs
                    in map (\x -> x - p) xs

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [_] = True
todosIguales xs = all (== head xs) xs

data AB a = Nil | Bin a (AB a) (AB a)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB (Bin _ _ _) = False

negacionAB :: AB Bool -> AB Bool -- dado un árbol de booleanos construye otro negado
negacionAB Nil = Nil
negacionAB (Bin r i d) = Bin (not r) (negacionAB i) (negacionAB d)