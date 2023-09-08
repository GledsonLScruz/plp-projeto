module EAN13 where

import Control.Monad (replicateM)
import Data.Char (digitToInt, intToDigit)
import System.Random (randomRIO)

-- Gera um código EAN-13 aleatório
gerarCodigoEAN13 :: IO String
gerarCodigoEAN13 = do
  -- Gera 12 dígitos aleatórios
  let gerarDigitos = replicateM 12 (randomRIO ('0', '9'))
  
  -- Gere 12 dígitos aleatórios
  digitos <- gerarDigitos
  
  -- Calcule o dígito de controle
  let digitoControle = calcularDigitoControle digitos
  
  -- Combine os dígitos e o dígito de controle
  let codigoEAN13 = digitos ++ [digitoControle]
  
  return codigoEAN13

-- Calcula o dígito de controle para um código EAN-13
calcularDigitoControle :: String -> Char
calcularDigitoControle digitos =
  let pesos = [1, 3] -- Pesos alternados para cálculo
      digitosComPesos = zipWith (\d p -> (digitToInt d) * p) (reverse digitos) (cycle pesos)
      soma = sum digitosComPesos
      digitoControle = intToDigit $ (10 - (soma `mod` 10)) `mod` 10
  in digitoControle

main :: IO ()
main = do
  codigoEAN13 <- gerarCodigoEAN13
  putStrLn $ "Código EAN-13 gerado: " ++ codigoEAN13
