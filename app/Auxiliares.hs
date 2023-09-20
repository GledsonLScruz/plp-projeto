{-# OPTIONS_GHC -Wno-missing-fields #-}
module Auxiliares where

import Produto
import Cliente

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
  putStrLn "Digite o nome completo do cliente:"
  nomeCompleto <- getLine
  putStrLn "Digite o sexo do cliente (M/F):"
  sexoStr <- getLine
  let sexo = head sexoStr
  putStrLn "Digite a data de nascimento do cliente:"
  dataNascimento <- getLine
  putStrLn "Digite o CPF do cliente:"
  cpf <- getLine
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
