valorAbsoluto :: Float -> Float
valorAbsoluto x = if x < 0 then -x else x

bisiesto :: Int -> Bool
bisiesto x = x `mod` 4 == 0

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

aEntero :: Either Int Bool -> Int
aEntero (Left x) = x
aEntero (Right True) = 1
aEntero (Right False) = 0

data AB a = Nil | Bin a (AB a) (AB a)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB (Bin _ _ _) = False

