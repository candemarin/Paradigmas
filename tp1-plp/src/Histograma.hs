-- | Un `Histograma` es una estructura de datos que permite contar cuántos valores hay en cada rango.
-- @vacio n (a, b)@ devuelve un histograma vacío con n+2 casilleros:
--
-- * @(-inf, a)@
-- * @[a, a + tamIntervalo)@
-- * @[a + tamIntervalo, a + 2*tamIntervalo)@
-- * ...
-- * @[b - tamIntervalo, b)@
-- * @[b, +inf)@
--
-- `vacio`, `agregar` e `histograma` se usan para construir un histograma.
module Histograma
  ( Histograma, -- No se exportan los constructores
    vacio,
    agregar,
    histograma,
    Casillero (..),
    casMinimo,
    casMaximo,
    casCantidad,
    casPorcentaje,
    casilleros,
  )
where

import Data.List
import Util

data Histograma = Histograma Float Float [Int]
  deriving (Show, Eq)

-- | Inicializa un histograma vacío con @n@ casilleros para representar
-- valores en el rango y 2 casilleros adicionales para los valores fuera del rango.
-- Require que @l < u@ y @n >= 1@.
vacio :: Int -> (Float, Float) -> Histograma
-- vacio n (l, u) = error "COMPLETAR EJERCICIO 3"
vacio n (l, u) = Histograma l ((u-l)/ fromIntegral n) ([0] ++ replicate n 0 ++ [0])

indice :: Float -> Histograma -> Int
indice x h@(Histograma l t cs) = max 0 $ min (length cs - 1) (floor ((x - l) / t) + 1)

-- | Agrega un valor al histograma.
agregar :: Float -> Histograma -> Histograma
agregar x h@(Histograma l t cs) 
   | x == infinitoPositivo = agregar (fromIntegral (length cs) * t + l + 1) h
   | x == infinitoNegativo = agregar (l - 1) h
   | otherwise = Histograma l t (actualizarElem (indice x h) (+1) cs)


-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma n r xs = foldr agregar (vacio n r) xs


-- | Un `Casillero` representa un casillero del histograma con sus límites, cantidad y porcentaje.
-- Invariante: Sea @Casillero m1 m2 c p@ entonces @m1 < m2@, @c >= 0@, @0 <= p <= 100@
data Casillero = Casillero Float Float Int Float
  deriving (Show, Eq)

-- | Mínimo valor del casillero (el límite inferior puede ser @-inf@)
casMinimo :: Casillero -> Float
casMinimo (Casillero m _ _ _) = m

-- | Máximo valor del casillero (el límite superior puede ser @+inf@)
casMaximo :: Casillero -> Float
casMaximo (Casillero _ m _ _) = m

-- | Cantidad de valores en el casillero. Es un entero @>= 0@.
casCantidad :: Casillero -> Int
casCantidad (Casillero _ _ c _) = c

-- | Porcentaje de valores en el casillero respecto al total de valores en el histograma. Va de 0 a 100.
casPorcentaje :: Casillero -> Float
casPorcentaje (Casillero _ _ _ p) = p

-- | Dado un histograma, devuelve la lista de casilleros con sus límites, cantidad y porcentaje.
casilleros :: Histograma -> [Casillero]
casilleros h = zipWith4 (\inicio fin cantidad porcentaje -> Casillero inicio fin cantidad porcentaje) (inicios h) (fines h) (cantidades h) (porcentajes h)

inicios :: Histograma -> [Float]
inicios (Histograma i t cs) = infinitoNegativo : [i + (fromIntegral indice * t) | indice <- [0 .. (length cs - 2)]]

fines :: Histograma -> [Float]
fines (Histograma i t cs) = tail (inicios (Histograma i t cs)) ++ [infinitoPositivo]

cantidades :: Histograma -> [Int]
cantidades (Histograma i t cs) = cs

porcentajes :: Histograma -> [Float]
porcentajes (Histograma i t cs) = let total = fromIntegral (sum cs) in
  if total == 0 then replicate (length cs) 0
  else map (\x -> fromIntegral x/total * 100) cs