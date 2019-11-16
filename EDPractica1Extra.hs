-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º ETSI Informática. UMA
-- Práctica 1 - Ejercicios extra
--
-- Alumno: Montalvo Lafuente, Rocio
-------------------------------------------------------------------------------

module Practica1Extra where

import           Test.QuickCheck

----------------------------------------------------------------------
-- Ejercicio - esPrimo
----------------------------------------------------------------------

esPrimo :: Integer -> Bool
esPrimo 1= True
esPrimo n |x<= 0 = error "el numero es negativo o 0"
          |otherwhise = aux 2 n 
    where 
        aux cont p |cont ==p =True 
                   | mod p cont == 0 = False
                   |otherwhise= aux (cont +1) p
----------------------------------------------------------------------
-- Ejercicio - cocienteYResto
----------------------------------------------------------------------

cocienteYResto :: Integer -> Integer->(Integer,Integer)
cocienteYResto x 0 = error "no se puede dividir por 0 "
cocienteYResto x y |x<0 || y <0 = error "los numeos no son naturales"
                   |otherwhise aux y 0 x 
    where 
        aux divdendo cociente resto |resto < dividendo = (cociente, resto)
                                    |otherwhise = aux dividendo (cociente +1) (resto-dividendo)


prop_cocienteYResto_OK x y = x==y*(div x y)+(mod x y)

-- Ejercicio - libre de cuadrados
----------------------------------------------------------------------

libreDeCuadrados :: Integer -> Bool
libreDeCuadrados n = aux n 2 
    where 
        aux x y | y ^2 >x  = True
                | mod x y^2 == 0 = False
                |otherwhise = aux x (y+1)


----------------------------------------------------------------------
-- Ejercicio - raiz entera
----------------------------------------------------------------------

raizEntera :: Integer -> Integer
raizEntera 0 =0 
raizEntera x |x<0 = error "No puedo hacer la raiz de un numero negativo"
             |otherwhise aux x 1 
where 
    aux x r | x<r^2 = (r-1)
            | otherwhise = aux x (r+1)


raizEnteraRapida :: Integer -> Integer
raizEnteraRapida n | n < 0 = error "No puedo hacer la raiz de un numero negativo"
                   | otherwise = aux n 0 n
  where   
   aux num ini fin | ini == fin = ini
                   | (ini+1) == fin && fin^2 > num = ini
                   | (ini+1) == fin = fin
                   | medio^2 == num = medio
                   | medio^2 > num = aux num ini medio
                   | otherwise = aux num medio fin  
       where medio = (ini + fin) `div` 2


----------------------------------------------------------------------
-- Ejercicio - números de Harshad
----------------------------------------------------------------------

sumaDigitos :: Integer -> Integer
sumaDigitos n = aux n 0 
  where
    aux n cont | n == 0 = cont
               | otherwise = aux (n `div` 10) (cont + n `mod` 10)


harshad :: Integer -> Bool
harshad x | x<=0= error "los numeros no son naturales"
          | otherwise = x `mod` sd == 0
  where sd = sumaDigitos x


harshadMultiple :: Integer -> Bool
harshadMultiple n = (harshad n) &&  (harshad (div n (sumaDigitos n))) 

vecesHarshad :: Integer -> Integer
vecesHarshad n = undefined

prop_Bloem_Harshad_OK :: Integer -> Property
prop_Bloem_Harshad_OK n = undefined

----------------------------------------------------------------------
-- Ejercicio - ceros del factorial
----------------------------------------------------------------------

factorial :: Integer -> Integer
factorial 0 = 1 
factorial n  = n +factorial (n-1)

cerosDe :: Integer -> Integer
cerosDe n | n `rem` 10 == 0 = 1 + cerosDe (n `div`10)
          | otherwise = 0
          
prop_cerosDe_OK :: Integer -> Integer -> Property
prop_cerosDe_OK n m = undefined

{-

Responde las siguientes preguntas:

¿En cuántos ceros acaba el factorial de 10?

¿En cuántos ceros acaba el factorial de 100?

¿En cuántos ceros acaba el factorial de 1000?

¿En cuántos ceros acaba el factorial de 10000?


-}

----------------------------------------------------------------------
-- Ejercicio - números de Fibonacci y fórmula de Binet
----------------------------------------------------------------------

fib :: Integer -> Integer
fib 0  = 0
fib 1 =1 
fib n = fib n + fib (n-1)

llamadasFib :: Integer -> Integer
llamadasFib n = undefined

{-

Responde a las siguientes preguntas:

¿Cuántas llamadas a fib son necesarias para calcular fib 30?


¿Cuántas llamadas a fib son necesarias para calcular fib 60?


-}

fib' :: Integer -> Integer
fib' n = undefined

prop_fib_OK :: Integer -> Property
prop_fib_OK n = undefined

phi :: Double
phi = undefined

binet :: Integer -> Integer
binet n = undefined

prop_fib'_binet_OK :: Integer -> Property
prop_fib'_binet_OK n = undefined

{-

Responde a la siguiente pregunta:

¿A partir de qué valor devuelve binet resultados incorrectos?

-}
