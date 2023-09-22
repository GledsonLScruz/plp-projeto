module ClienteRepository where

import Cliente

-- Adicione um cliente ao repositório
adicionarCliente :: [Cliente] -> Cliente -> [Cliente]
adicionarCliente repositorio cliente = cliente : repositorio

-- Função para buscar um cliente por cpf em uma lista de clientes
buscarClientePorCpf :: [Cliente] -> String -> Maybe Cliente
buscarClientePorCpf [] _ = Nothing
buscarClientePorCpf (cliente:clientes) cpf
  | cpf == getCpf cliente = Just cliente
  | otherwise = buscarClientePorCpf clientes cpf

-- Função para buscar um cliente por cpf em uma lista de clientes
buscarClientePorLogin :: [Cliente] -> String -> String -> Maybe Cliente
buscarClientePorLogin [] _ _ = Nothing
buscarClientePorLogin (cliente:clientes) email senha
  | email == getEmail cliente && senha == getSenha cliente = Just cliente
  | otherwise = buscarClientePorLogin clientes email senha


-- Função que remove um cliente do repositório
removerCliente :: [Cliente] -> String -> [Cliente]
removerCliente repositorio cpfCliente =
  filter (\cliente -> cpf cliente /= cpfCliente) repositorio

-- Função para atualizar um cliente especificado
atualizarCliente :: [Cliente] -> String -> Cliente -> [Cliente]
atualizarCliente [] _ _ = []
atualizarCliente (cliente:clientes) cpfCliente novoCliente
  | cpf cliente == cpfCliente = do
      novoCliente : clientes
  | otherwise = cliente : atualizarCliente clientes cpfCliente novoCliente

-- Função para um cliente atualizar o próprio cadastro
atualizarCadastroCliente :: [Cliente] -> String -> Cliente -> [Cliente]
atualizarCadastroCliente clientes cpfAtualizacao novoCliente =
  case buscarClientePorCpf clientes cpfAtualizacao of
    Just clienteExistente ->
      let clienteAtualizado = clienteExistente { 
            sexo = sexo novoCliente,
            dataNascimento = dataNascimento novoCliente,
            email = email novoCliente,
            telefone = telefone novoCliente,
            nomeUsuario = nomeUsuario novoCliente,
            senha = senha novoCliente
          }
          clientesAtualizados = atualizarCliente clientes cpfAtualizacao clienteAtualizado
      in clientesAtualizados
    Nothing -> clientes