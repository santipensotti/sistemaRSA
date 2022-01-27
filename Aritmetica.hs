module Aritmetica where
import Tipos
import Data.Tuple
import Data.Bits


--(0)
mcdExt :: Integer -> Integer -> (Integer, (Integer, Integer))
mcdExt n 0 = (abs n, (n,0))
mcdExt 0 b = (abs b, (0, b))
mcdExt n b = ( mcd n b, (n, b))

mcd :: Integer ->Integer ->Integer
mcd n b | mod n b == 0 = b
        | otherwise = mcd b (mod n b)

--(1)

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | mod n k == 0 = k
                      | otherwise = menorDivisorDesde n (k+1)

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = n == menorDivisor n


criba :: Integer -> Set Integer 
criba n | n <= 2 = [] -- Si el n es menor o igual a 2 no hay un primo menor a n y devuelvo una lista vacia
        | esPrimo (n-1) = (n-1):(criba (n-1)) -- Si el anterior es primo, lo agrego a la lista y llamo a la funcion de nuevo 
        | otherwise = criba (n-1) 


--(2)

-- Busco un numero < n-2 tal que sea coprimo con n llamando a una auxiliar
coprimoCon:: Integer -> Integer
coprimoCon n = coprimoNMenores (n) (n-2) 

coprimoNMenores :: Integer -> Integer -> Integer
coprimoNMenores n b 
           |  mcd n b == 1 = b  -- Si los dos numeros tienen mcd = 1, entonces son coprimos y devuelvo el numero
           |  otherwise = coprimoNMenores n (b-1)


--(3)

--Busco un numero que al multplicarlo por n y despues dividir el resultado por b, tenga resto 1
inversoMultiplicativo:: Integer -> Integer -> Integer
inversoMultiplicativo n b | mcd n b  == 1 = buscaD n  1 b -- Si los dos numeros son coprimos, entoces existe un numero que lo logre
                          | otherwise = 0

-- dado un n y b, busco un numero que al multlicarlo por n y el resultado lo divido por b sea resto 1
buscaD :: Integer -> Integer -> Integer -> Integer
buscaD n x b | mod (n*x) b == 1 = x
             | otherwise = buscaD n (x+1) b 



-- Función de regalo para exponenciar "rápido"
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1
