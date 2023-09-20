{-# OPTIONS_GHC -Wno-missing-fields #-}
module Auxiliares where

import Produto
import Cliente
import System.Random
import Data.List

-- Função auxiliar para ler um novo produto do usuário
lerProduto :: Int -> IO Produto
lerProduto idSistema = do
  putStrLn "Digite o nome do produto:"
  nomeProduto <- getLine
  putStrLn "Digite a categoria do produto:"
  categoriaProduto <- getLine
  putStrLn "Digite o preço do produto:"
  precoStr <- getLine
  let precoProduto = read precoStr :: Double
  putStrLn "Digite o preço de venda do produto:"
  precoVendaStr <- getLine
  let precoVendaProduto = read precoVendaStr :: Double
  putStrLn "Digite a quantidade inicial em estoque:"
  quantidadeStr <- getLine
  let quantidadeProduto = read quantidadeStr :: Int
  putStrLn "Digite a data de fabricação do produto:"
  dataFabricacao <- getLine
  putStrLn "Digite a data de validade do produto:"
  dataValidade <- getLine
  let id = idSistema
  return
    Produto
      { codigo = id
      , disponivel = True
      , nome = nomeProduto
      , categoria = categoriaProduto
      , precoCompra = precoProduto
      , precoVenda = precoVendaProduto
      , quantidade = quantidadeProduto
      , fabricacao = dataFabricacao
      , validade = dataValidade
      }

-- Função auxiliar para ler um novo cliente do usuário
lerCliente :: IO Cliente
lerCliente = do
  putStrLn $ "Digite o nome completo do cliente:"
  nomeCompleto <- getLine
  putStrLn $ "Digite o sexo do cliente (M/F):"
  sexoStr <- getLine
  let sexo = head sexoStr
  putStrLn $ "Digite a data de nascimento do cliente:"
  dataNascimento <- getLine
  putStrLn $ "Digite o CPF do cliente:"
  cpf <- getLine
  putStrLn $ "Digite o e-mail do cliente:"
  email <- getLine
  putStrLn $ "Digite o telefone do cliente:"
  telefone <- getLine
  putStrLn $ "Digite o nome de usuário do cliente:"
  nomeUsuario <- getLine
  putStrLn $ "Digite a senha do cliente:"
  senha <- getLine
  return
    Cliente
      { nomeCompleto = nomeCompleto
      , sexo = sexo
      , dataNascimento = dataNascimento
      , cpf = cpf
      , email = email
      , telefone = telefone
      , nomeUsuario = nomeUsuario
      , senha = senha
      }

-- Função auxiliar para ler um novo cliente do usuário
lerAtualizarCadastro :: IO Cliente
lerAtualizarCadastro = do
  putStrLn "Digite o sexo do cliente (M/F):"
  sexoStr <- getLine
  let sexo = head sexoStr
  putStrLn "Digite a data de nascimento do cliente:"
  dataNascimento <- getLine
  putStrLn "Digite o e-mail do cliente:"
  email <- getLine
  putStrLn "Digite o telefone do cliente:"
  telefone <- getLine
  putStrLn "Digite o nome de usuário do cliente:"
  nomeUsuario <- getLine
  putStrLn "Digite a senha do cliente:"
  senha <- getLine
  return
    Cliente
      { sexo = sexo
      , dataNascimento = dataNascimento
      , email = email
      , telefone = telefone
      , nomeUsuario = nomeUsuario
      , senha = senha
      }

-- Crie uma lista de produtos (um repositório)
criarRepositorioProdutosExemplo :: [Produto]
criarRepositorioProdutosExemplo = [
  Produto {codigo = 1, disponivel = True, nome = "Arroz", categoria = "Alimento", precoCompra = 10.0, precoVenda = 15.0, quantidade = 100, fabricacao = "01/01/2021", validade = "01/01/2022"},
  Produto {codigo = 2, disponivel = True, nome = "Feijão", categoria = "Alimento", precoCompra = 5.0, precoVenda = 10.0, quantidade = 50, fabricacao = "01/01/2021", validade = "01/01/2022"},
  Produto {codigo = 3, disponivel = True, nome = "Coca-Cola", categoria = "Bebida", precoCompra = 5.0, precoVenda = 10.0, quantidade = 50, fabricacao = "01/01/2021", validade = "01/01/2022"},
  Produto {codigo = 4, disponivel = True, nome = "Pepsi", categoria = "Bebida", precoCompra = 5.0, precoVenda = 10.0, quantidade = 50, fabricacao = "01/01/2021", validade = "01/01/2022"},
  Produto {codigo = 5, disponivel = True, nome = "Sabão em pó", categoria = "Limpeza", precoCompra = 5.0, precoVenda = 10.0, quantidade = 50, fabricacao = "01/01/2021", validade = "01/01/2022"},
  Produto {codigo = 6, disponivel = True, nome = "Sabão em barra", categoria = "Limpeza", precoCompra = 5.0, precoVenda = 10.0, quantidade = 50, fabricacao = "01/01/2021", validade = "01/01/2022"},
  Produto {codigo = 7, disponivel = True, nome = "Detergente", categoria = "Limpeza", precoCompra = 5.0, precoVenda = 10.0, quantidade = 50, fabricacao = "01/01/2021", validade = "01/01/2022"},
  Produto {codigo = 8, disponivel = True, nome = "Desinfetante", categoria = "Limpeza", precoCompra = 5.0, precoVenda = 10.0, quantidade = 50, fabricacao = "01/01/2021", validade = "01/01/2022"}]

-- Crie uma lista de clientes (um repositório)
criarRepositorioClientesExemplo :: [Cliente]
criarRepositorioClientesExemplo = [
    Cliente {nomeCompleto = "João da Silva", sexo = 'M', dataNascimento = "01/01/1996", cpf = "123.456.789-00", email = "joao.silva@gmail.com", telefone = "(11) 99999-9999", nomeUsuario = "joao.silva", senha = "123456"},
    Cliente {nomeCompleto = "Maria da Silva", sexo = 'F', dataNascimento = "07/07/1990", cpf = "123.456.789-00", email = "maria.silva20@gmail.com", telefone = "(81) 7676-7899", nomeUsuario = "maria.silva", senha = "987654"},
    Cliente {nomeCompleto = "José Gomes", sexo = 'M', dataNascimento = "05/01/2000", cpf = "123.456.789-00", email = "jose.gomes@hotmail.com", telefone = "(21) 1234-5678", nomeUsuario = "jose.gomes", senha = "123asd"},
    Cliente {nomeCompleto = "Ana Maria", sexo = 'F', dataNascimento = "01/01/1996", cpf = "123.456.789-00", email = "ana@outlook.com", telefone = "(11) 99999-9999", nomeUsuario = "ana.maria", senha = "123456"}]


-- Function to generate a random 12-digit number
generateRandom12DigitNumber :: IO String
generateRandom12DigitNumber = do
    gen <- newStdGen
    let randomNumber = take 12 $ randomRs ('0', '9') gen
    return randomNumber

-- Function to calculate the EAN-13 checksum digit
calculateEAN13Checksum :: [Int] -> Int
calculateEAN13Checksum digits =
    let odds = sum (map snd (filter (\(i, _) -> i `mod` 2 /= 0) (zip [1..] digits))) * 3
        evens = sum (map snd (filter (\(i, _) -> i `mod` 2 == 0) (zip [1..] digits)))
    in (10 - ((odds + evens) `mod` 10)) `mod` 10

-- Function to generate the complete EAN-13 code
generateEAN13 :: String -> String
generateEAN13 input =
    let digits = read input
	checksum = calculateEAN13Checksum digits
	in input ++ show checksum
