module ClienteService where

import Cliente
import ClienteRepository

-- Função para adicionar um cliente ao repositório
adicionarClienteService :: [Cliente] -> Cliente -> [Cliente]
adicionarClienteService repositorio cliente = adicionarCliente repositorio cliente

-- Função para buscar um cliente por cpf no repositório
buscarClientePorCpfService :: [Cliente] -> String -> Maybe Cliente
buscarClientePorCpfService repositorio cpf = buscarClientePorCpf repositorio cpf

-- Função para remover um cliente do repositório
removerClienteService :: [Cliente] -> String -> [Cliente]
removerClienteService repositorio cpf = removerCliente repositorio cpf

-- Função para atualizar um cliente no repositório
atualizarClienteService :: [Cliente] -> String -> Cliente -> [Cliente]
atualizarClienteService repositorio cpf cliente = atualizarCliente repositorio cpf cliente

-- Função para um cliente atualizar o próprio cadastro
atualizarCadastroClienteService :: [Cliente] -> String -> Cliente -> [Cliente]
atualizarCadastroClienteService clientes cpfAtualizacao novoCliente =
  case buscarClientePorCpfService clientes cpfAtualizacao of
    Just clienteExistente ->
      let clienteAtualizado = clienteExistente { 
            sexo = sexo novoCliente,
            dataNascimento = dataNascimento novoCliente,
            email = email novoCliente,
            telefone = telefone novoCliente,
            nomeUsuario = nomeUsuario novoCliente,
            senha = senha novoCliente
          }
          clientesAtualizados = atualizarClienteService clientes cpfAtualizacao clienteAtualizado
      in clientesAtualizados
    Nothing -> clientes




