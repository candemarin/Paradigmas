module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar,
  )
where

import Generador
import Histograma

-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

recrExpr :: (Float -> a) -> (Float -> Float -> a) -> (a -> a -> Expr -> Expr -> a) -> (a -> a -> Expr -> Expr -> a) -> (a -> a -> Expr -> Expr -> a) -> (a -> a -> Expr -> Expr -> a) -> Expr -> a
recrExpr f_const f_rango f_suma f_resta f_mult f_div expr =
  let re = recrExpr f_const f_rango f_suma f_resta f_mult f_div
  in case expr of Const x -> f_const x
                  Rango x y -> f_rango x y
                  Suma x y -> f_suma (re x) (re y) x y
                  Resta x y -> f_resta (re x) (re y) x y
                  Mult x y -> f_mult (re x) (re y) x y
                  Div x y -> f_div (re x) (re y) x y

foldExpr :: (Float -> a) -> (Float -> Float -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
foldExpr f_const f_rango f_suma f_resta f_mult f_div expr =
  let fold = foldExpr f_const f_rango f_suma f_resta f_mult f_div
  in case expr of Const x -> f_const x
                  Rango x y -> f_rango x y
                  Suma x y -> f_suma (fold x) (fold y)
                  Resta x y -> f_resta (fold x) (fold y)
                  Mult x y -> f_mult (fold x) (fold y)
                  Div x y -> f_div (fold x) (fold y)


-- | Evaluar expresiones dado un generador de números aleatorios
-- type G a = Gen -> (a, Gen)
eval :: Expr -> G Float
eval expr = foldExpr
              (\x gen -> (x, gen))
              (\x1 x2 gen -> dameUno (x1, x2) gen)
              (res (+))
              (res (-))
              (res (*))
              (res (/))
              expr
              where
                  res f g1 g2 gen =
                    let (r1, gen') = g1 gen
                        (r2, gen'') = g2 gen'
                    in (f r1 r2, gen'')


-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = (histograma m (rango95 (fst muestraGen)) (fst muestraGen), (snd muestraGen))
                          where muestraGen = muestra f n g

-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = armarHistograma m n (eval expr)

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
--- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)
-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar = recrExpr
              (\v -> show v)
              (\x1 x2 -> show x1 ++ "~" ++ show x2)
              (\v1 v2 exp1 exp2 -> parenConmutativos exp1 CESuma v1 ++ " + " ++ parenConmutativos exp2 CESuma v2)
              (\v1 v2 exp1 exp2 -> parenNoConmutativos exp1 v1 ++ " - " ++ parenNoConmutativos exp2 v2)
              (\v1 v2 exp1 exp2 -> parenConmutativos exp1 CEMult v1 ++ " * " ++ parenConmutativos exp2 CEMult v2)
              (\v1 v2 exp1 exp2 -> parenNoConmutativos exp1 v1 ++ " / " ++ parenNoConmutativos exp2 v2)
              where parenConmutativos exp c = maybeParen (constructor exp `notElem` [c, CEConst, CERango])
                    parenNoConmutativos exp = maybeParen (constructor exp `elem` [CESuma, CEResta])

data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)

-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv

-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s
