module RSA where
import Tipos
import Aritmetica


--(4)

-- Dados dos primos, devuelvo las claves privadas con la formula de RSA
claves :: Integer -> Integer -> (Integer, Integer, Integer)
claves p q  = (e, d, n)
  where 
        x = (p-1)*(q-1)
        n = p*q 
        e = coprimoCon x
        d = buscaD e 0 x


--(5)

-- Convierto el mensaje a numeros y llamo una funcion auxiliar que lo codifique
codificador :: Clpub -> Mensaje -> Cifrado
codificador n b | ascii == [] = []
                |  otherwise = codificoYAgrego n ascii
       where ascii = aEnteros b         

-- Si el mcd entre el n y el mensaje es igual a 1, lo codifico segun la formula y luego lo agrego a una lista
-- En caso contrario codifico el mensaje, cambiandole el signo y luego lo agrego a lista y llamo nuevamente a la funcion
codificoYAgrego :: Clpub -> [Integer] -> Cifrado
codificoYAgrego k [] = []
codificoYAgrego k (x:xs) | mcd n x == 1 = (modExp x e n) : (codificoYAgrego k xs) 
                      | otherwise = -x : (codificoYAgrego k xs)
        where e = fst k
              n = snd k


--(6)

--Llamo a una funcion que decodifique el mensaje y convierto esa lista a caracteres
decodificador :: Clpri -> Cifrado -> Mensaje
decodificador n b | b == [] = ""
                  | otherwise = aChars (defodicoYAgrego n b)

-- Segun la formula de RSA como decodifico el mensaje y despues lo agrego a una lista vacia y llamo a la funcion nuevamente recorriendo el mensaje
defodicoYAgrego :: Clpri -> [Integer] -> Set Integer
defodicoYAgrego k [] = []
defodicoYAgrego k (x:xs) | mcd n x == 1 = (modExp x d n) : (defodicoYAgrego k xs)
                        | otherwise = -x : (defodicoYAgrego k xs )
      where d = fst k
            n = snd k