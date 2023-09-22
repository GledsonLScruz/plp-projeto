module Controllers where

import Produto
import Cliente
import ProdutoRepository
import ClienteRepository
import Auxiliares
import Tabela
import Dashboards
import Data.Char
import CarrinhoService

-- Controller inicial do sistema
initialController :: [Produto] -> [Cliente] -> Int -> [Produto] -> IO ()
initialController produtos clientes codigoProduto historicoCompras = do
  putStrLn $
    "=====================================\n" ++
    "         Menu de Visitante          \n" ++
    "=====================================\n\n" ++
    "  (01) Entrar como Cliente\n" ++
    "  (02) Registrar como Cliente\n" ++
    "  (03) Entrar como Administrador\n" ++
    "  (04) Visualizar Produtos\n" ++
    "  (05) Sair do Sistema\n"

  putStrLn "Digite a opção desejada: "
  opcao <- getLine

  case opcao of
    "01" -> loginController produtos clientes codigoProduto historicoCompras

    "02" -> registrerController produtos clientes codigoProduto historicoCompras

    "03" -> loginADMController produtos clientes codigoProduto historicoCompras

    "04" -> do
      mapM_ (putStrLn . produtoToStringDTO) produtos
      initialController produtos clientes codigoProduto historicoCompras

    "05" -> do
      putStrLn "Saindo do sistema."

    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      initialController produtos clientes codigoProduto historicoCompras

-- LoginController guarda os comandos de Login
loginController :: [Produto] -> [Cliente] -> Int -> [Produto] -> IO ()
loginController produtos clientes codigoProduto historicoCompras = do
  putStrLn $ "Faça Login como Cliente:\nEmail:\n"
  email <- getLine
  putStrLn $ "Senha:\n"
  senha <- getLine
  let carrinhoVazio = novoCarrinhoVazio
  let usuario = buscarClientePorLogin clientes email senha
  case usuario of
    Just l -> do
      clienteController produtos clientes codigoProduto l carrinhoVazio historicoCompras
    Nothing -> do
      putStrLn $ "\nLogin Inválido tente novamente\n"
      initialController produtos clientes codigoProduto historicoCompras

-- LoginController guarda os comandos de Login
loginADMController :: [Produto] -> [Cliente] -> Int -> [Produto] -> IO ()
loginADMController produtos clientes codigoProduto historicoCompras= do
  putStrLn $ "Faça Login como ADM:\n nome de usuário:\n"
  user <- getLine
  putStrLn $ "Senha:\n"
  senha <- getLine
  if user == "admin" && senha == "password"
    then do
      putStrLn $ "Login como ADM feito com sucesso!"
      admController produtos clientes codigoProduto historicoCompras
    else do
      putStrLn $ "\nLogin Inválido tente novamente\n"
      initialController produtos clientes codigoProduto historicoCompras


-- RegisterController guarda os comandos de Register
registrerController :: [Produto] -> [Cliente] -> Int -> [Produto] -> IO ()
registrerController produtos clientes codigoProduto historicoCompras = do
  putStrLn $ "Cadastre um novo Cliente:\n"
  novoCliente <- lerCliente
  let clientes = adicionarCliente clientes novoCliente 
  putStrLn $ "Cliente Cadastrado"
  salvarClientes clientes
  let carrinhoVazio = novoCarrinhoVazio
  clienteController produtos clientes codigoProduto novoCliente carrinhoVazio historicoCompras


-- Controller que guarda comandos do cliente
clienteController :: [Produto] -> [Cliente] -> Int -> Cliente -> CarrinhoCompra -> [Produto] -> IO ()
clienteController produtos clientes codigoProduto clienteLogado carrinho historicoCompras = do
  putStrLn $
    "=====================================\n" ++
    "        Menu de Cliente             \n" ++
    "=====================================\n\n" ++
    "  (01) Visualizar Produtos\n" ++
    "  (02) Visualizar Produtos por Categoria\n" ++
    "  (03) Adicionar ao Carrinho\n" ++
    "  (04) Remover do Carrinho\n" ++
    "  (05) Visualizar Carrinho\n" ++
    "  (06) Finalizar Compra\n" ++
    "  (07) Avaliar Produto\n" ++
    "  (08) Histórico de Compra\n" ++
    "  (09) Atualizar Meu Cadastro\n" ++
    "  (10) Deletar Minha Conta\n" ++
    "  (11) Sair do Modo Cliente\n" ++
    "  (12) Sair do Sistema\n"

  putStrLn "Digite a opção desejada: "
  opcao <- getLine

  case opcao of
    "01" -> do
      mapM_ (putStrLn . produtoToStringDTO) produtos
      clienteController produtos clientes codigoProduto clienteLogado carrinho historicoCompras

    "02" -> do
        putStrLn "Digite a categoria a ser buscada:"
        categoria <- getLine
        let produtosEncontrados = buscarProdutosPorCategoria produtos categoria
        mapM_ (putStrLn . produtoToStringDTO) produtosEncontrados
        clienteController produtos clientes codigoProduto clienteLogado carrinho historicoCompras

    "03" -> do
      putStrLn "Digite o codigo do produto que você deseja adicionar ao Carrinho:"
      codigoProdutoStr <- getLine
      let codigoProduto = read codigoProdutoStr :: Int
      let produto = buscarProdutoPorCodigo produtos codigoProduto
      case produto of
        Just p -> do
          putStrLn $ "O produto:\n" ++ produtoToString p ++ "\nFoi adicionado ao Carrinho!!\n"
          let carrinhoAtualizado = adicionarProdutoCarrinho carrinho p
          clienteController produtos clientes codigoProduto clienteLogado carrinhoAtualizado historicoCompras
        Nothing -> do
          putStrLn "Produto não encontrado."
          clienteController produtos clientes codigoProduto clienteLogado carrinho historicoCompras

    "04" -> do
      putStrLn "Digite o codigo do produto que você deseja remover do Carrinho:"
      codigoProdutoStr <- getLine
      let codigoProduto = read codigoProdutoStr :: Int
      let produto = buscarProdutoPorCodigo produtos codigoProduto
      case produto of
        Just p -> do
          putStrLn $ "O produto:\n" ++ produtoToString p ++ "\nFoi removido do Carrinho!!\n"
          let carrinhoAtualizado = removerProdutoCarrinho carrinho codigoProduto
          clienteController produtos clientes codigoProduto clienteLogado carrinhoAtualizado historicoCompras
        Nothing -> do
          putStrLn "Produto não encontrado."
          clienteController produtos clientes codigoProduto clienteLogado carrinho historicoCompras

    "05" -> do
      putStrLn "Carrinho de Compras:"
      let produtosCarrinho = getCarrinhoProdutos carrinho
      mapM_ (putStrLn . produtoToStringDTO) produtosCarrinho
      clienteController produtos clientes codigoProduto clienteLogado carrinho historicoCompras

    "06" -> do
      putStrLn ("O valor total da compra é: " ++ show (calcularTotal carrinho) ++ "\n")
      putStrLn "insira o pagamento para finalizar a compra: "
      pagamentoStr <- getLine
      let pagamento = read pagamentoStr :: Double
      if pagamento == calcularTotal carrinho
        then do
          let produtosCarrinho = getCarrinhoProdutos carrinho
          let historicoComprasAtualizado = adicionarProdutosHistorico historicoCompras produtosCarrinho
          let produtosAtualizado = removerUnidadesProduto produtos produtosCarrinho
          salvarHistoricoCompras historicoComprasAtualizado
          putStrLn "Compra finalizada com sucesso!\nObrigado por comprar conosco!\n"
          let carrinhoVazio = novoCarrinhoVazio
          clienteController produtosAtualizado clientes codigoProduto clienteLogado carrinhoVazio historicoComprasAtualizado
        else do
          putStrLn "Pagamento insuficiente!"

      clienteController produtos clientes codigoProduto clienteLogado carrinho historicoCompras

    "07" -> do
      putStrLn "Falta implementar Avaliar Produto"
      clienteController produtos clientes codigoProduto clienteLogado carrinho historicoCompras

    "08" -> do
      putStrLn "Falta implementar Histórico de Compra"
      clienteController produtos clientes codigoProduto clienteLogado carrinho historicoCompras

    "09" -> do
      putStrLn "Digite o seu cpf para atualizar o cadastro:"
      cpf <- getLine
      novoCliente <- lerAtualizarCadastro
      let clientesAtualizados = atualizarCadastroCliente clientes cpf novoCliente
      putStrLn "Cadastro atualizado com sucesso."
      clienteController produtos clientesAtualizados codigoProduto clienteLogado carrinho historicoCompras

    "10" -> do
        putStrLn "Digite o seu cpf para deletar a conta:"
        cpf <- getLine
        let clientesAtualizados = removerCliente clientes cpf
        putStrLn "Conta deletada com sucesso."
        initialController produtos clientesAtualizados codigoProduto historicoCompras

    "11" -> do
      initialController produtos clientes codigoProduto historicoCompras

    "12" -> putStrLn "Saindo do sistema."

    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      clienteController produtos clientes codigoProduto clienteLogado carrinho historicoCompras

carrinhoController :: [Produto] -> [Cliente] -> Int -> Cliente -> CarrinhoCompra -> [Produto] -> IO ()
carrinhoController produtos clientes codigoProduto clienteLogado carrinho historicoCompras = do
  putStrLn "Insira o código do produto que você deseja adicionar ao Carrinho:"
  codigoProdutoStr <- getLine
  let codigoProduto = read codigoProdutoStr :: Int
  let produto = buscarProdutoPorCodigo produtos codigoProduto
  case produto of
    Just p -> do
      putStrLn $ "O produto:\n" ++ produtoToString p ++ "\nFoi adicionado ao Carrinho!!\n"
      let carrinhoAtualizado = adicionarProdutoCarrinho carrinho p
      clienteController produtos clientes codigoProduto clienteLogado carrinhoAtualizado historicoCompras
    Nothing -> do
      putStrLn "Produto não encontrado."
      clienteController produtos clientes codigoProduto clienteLogado carrinho historicoCompras


-- Controller que aguarda comandos do administrador
admController :: [Produto] -> [Cliente] -> Int -> [Produto] -> IO ()
admController produtos clientes codigoProduto historicoCompras = do
  putStrLn $
    "=====================================\n" ++
    "     Menu de Administrador           \n" ++
    "=====================================\n\n" ++
    "  (01) Visualizar Produtos\n" ++
    "  (02) Adicionar Novo Produto\n" ++
    "  (03) Atualizar Produto por Completo\n" ++
    "  (04) Visualizar Produto por Código\n" ++
    "  (05) Visualizar Produtos por Categoria\n" ++
    "  (06) Remover Produto por Código\n" ++
    "  (07) Ler Cliente por CPF\n" ++
    "  (08) Atualizar Cliente Completo\n" ++
    "  (09) Deletar Cliente por CPF\n" ++
    "  (10) Visualizar Dashboard\n" ++
    "  (11) Sair do Modo Administrador\n" ++
    "  (12) Sair do Sistema\n"

  putStrLn "Digite a opção desejada: "
  opcao <- getLine

  case opcao of
    "01" -> do
      mapM_ (putStrLn . produtoToString) produtos
      admController produtos clientes codigoProduto historicoCompras

    "02" -> do
      novoProduto <- lerProduto codigoProduto
      let produtosAtualizados = adicionarProduto produtos novoProduto
      putStrLn "Produto adicionado com sucesso."
      salvarProdutos produtosAtualizados
      let codigoAtualizado = codigoProduto + 1
      salvarCodigo codigoAtualizado
      admController produtosAtualizados clientes codigoAtualizado historicoCompras

    "03" -> do
      putStrLn "Digite o código do produto a ser atualizado:"
      codigoProdutoStr <- getLine
      let codigoProduto = read codigoProdutoStr :: Int
      novoProduto <- lerProduto codigoProduto
      let produtosAtualizados = atualizarProduto produtos codigoProduto novoProduto
      putStrLn "Produto atualizado com sucesso."
      salvarProdutos produtosAtualizados
      admController produtosAtualizados clientes codigoProduto historicoCompras

    "04" -> do
      putStrLn "Digite o código do produto a ser visualizado:"
      codigoProdutoStr <- getLine
      let codigoProduto = read codigoProdutoStr :: Int
      let produto = buscarProdutoPorCodigo produtos codigoProduto
      case produto of
        Just p -> do
          putStrLn $ produtoToString p
          admController produtos clientes codigoProduto historicoCompras
        Nothing -> do
          putStrLn "Produto não encontrado."
          admController produtos clientes codigoProduto historicoCompras

    "05" -> do
      putStrLn "Digite a categoria a ser buscada:"
      categoria <- getLine
      let produtosEncontrados = buscarProdutosPorCategoria produtos categoria
      mapM_ (putStrLn . produtoToString) produtosEncontrados
      admController produtos clientes codigoProduto historicoCompras

    "06" -> do
      putStrLn "Digite o código do produto a ser removido:"
      codigoProdutoStr <- getLine
      let codigoProduto = read codigoProdutoStr :: Int
      let produtosAtualizados = removerProduto produtos codigoProduto
      putStrLn "Produto removido com sucesso."
      salvarProdutos produtosAtualizados
      admController produtosAtualizados clientes codigoProduto historicoCompras

    "07" -> do
        putStrLn "Digite o CPF do cliente a ser buscado:"
        cpfCliente <- getLine
        let cliente = buscarClientePorCpf clientes cpfCliente
        case cliente of
            Just c -> do
                putStrLn $ clienteToString c
                admController produtos clientes codigoProduto historicoCompras
            Nothing -> do
                putStrLn "Cliente não encontrado."
                admController produtos clientes codigoProduto historicoCompras

    "08" -> do
        putStrLn "Digite o CPF do cliente a ser atualizado:"
        cpfCliente <- getLine
        novoCliente <- lerCliente
        let clientesAtualizados = atualizarCliente clientes cpfCliente novoCliente
        putStrLn "Cliente atualizado com sucesso."
        salvarClientes clientesAtualizados
        admController produtos clientes codigoProduto historicoCompras

    "09" -> do
        putStrLn "Digite o CPF do cliente a ser removido:"
        cpfCliente <- getLine
        let clientesAtualizados = removerCliente clientes cpfCliente
        putStrLn "Cliente removido com sucesso."
        salvarClientes clientesAtualizados
        admController produtos clientes codigoProduto historicoCompras

    "10" -> do
      exibirDashboards produtos clientes codigoProduto historicoCompras

    "11" -> do
      initialController produtos clientes codigoProduto historicoCompras

    "12" -> putStrLn "Saindo do sistema."

    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      admController produtos clientes codigoProduto historicoCompras

-- Função para exibir os dashboards
exibirDashboards :: [Produto] -> [Cliente] -> Int -> [Produto] -> IO ()
exibirDashboards produtos clientes codigoProduto historicoCompras = do
  putStrLn $ "Escolha um dashboard para exibir:\n" ++
          "01. Total de Clientes\n" ++
          "02. Clientes Mais Ativos\n" ++
          "03. Média de Compras por Cliente\n" ++
          "04. Quantidade de produtos em estoque \n" ++
          "05. Produtos com estoque baixo \n" ++
          "06. Produtos mais populares \n" ++
          "07. Total de receita gerada por produto \n" ++
          "08. Total de receita gerada por categoria \n" ++
          "09. Voltar\n"

  opcao <- getLine

  case opcao of
    "01" -> do
      putStrLn "Dashboard: Total de Clientes:"
      putStrLn "-------------"
      -- Exiba o dashboard de Total de Clientes
      let totalClientes = length clientes
      putStrLn $ "Total de Clientes: " ++ show totalClientes
      putStrLn "-------------"
<<<<<<< HEAD
      exibirDashboards produtos clientes idProduto

=======
      exibirDashboards produtos clientes codigoProduto historicoCompras
{-
>>>>>>> main
    "02" -> do
      putStrLn "Dashboard: Clientes Mais Ativos:"
      putStrLn "-------------"
      -- Exiba o dashboard de Clientes Mais Ativos
      let clientesAtivos = clientesMaisAtivos clientes
      putStrLn "Clientes Mais Ativos:"
      mapM_ (\cliente -> putStrLn $ "  - " ++ nomeCompleto cliente ++ ": " ++ show (length (historicoCompras cliente)) ++ " compras") clientesAtivos
      putStrLn "-------------"
      exibirDashboards produtos clientes codigoProduto historicoCompras

    "03" -> do
      putStrLn "Dashboard: Média de Compras por Cliente:"
      putStrLn "-------------"
      -- Exiba o dashboard de Média de Compras por Cliente
      let mediaCompras = mediaComprasPorCliente clientes
      putStrLn $ "Média de Compras por Cliente: " ++ show mediaCompras
      putStrLn "-------------"
<<<<<<< HEAD
      exibirDashboards produtos clientes idProduto

=======
      exibirDashboards produtos clientes codigoProduto historicoCompras
-}
>>>>>>> main
    "04" -> do
      putStrLn "Dashboard: Quantidade de produtos em estoque:"
      putStrLn "-------------"
      -- Exiba o dashboard de Quantidade de Produtos em Estoque
      let totalEstoque = quantidadeTotalEstoque produtos
      putStrLn $ "Quantidade total de produtos em estoque: " ++ show totalEstoque
      putStrLn "-------------"
      exibirDashboards produtos clientes codigoProduto historicoCompras

    "05" -> do
      putStrLn "Dashboard: Produtos com estoque baixo:"
      putStrLn "-------------"
      let produtosBaixosEstoque = produtosComEstoqueBaixo produtos
      mapM_ (\p -> putStrLn $ "  - " ++ getNome p ++ ": " ++ show (getQuantidade p)) produtosBaixosEstoque
      putStrLn "-------------"
      exibirDashboards produtos clientes codigoProduto historicoCompras

    "06" -> do
      {-
      putStrLn "Dashboard: Produtos Mais Populares:"
      putStrLn "-------------"
      -- Exiba o dashboard de Produtos Mais Populares
      let produtosPopulares = produtosMaisPopulares produtos
      putStrLn "Produtos Mais Populares:"
      mapM_ (\produto -> putStrLn $ "  - " ++ nome produto ++ ": " ++ show (quantidadeVendida produto) ++ " unidades vendidas") produtosPopulares
      -}
      putStrLn "-------------"
      exibirDashboards produtos clientes codigoProduto historicoCompras

    "07" -> do
      putStrLn "Dashboard: Total de receita gerada por produto:"
      putStrLn "-------------"
      let receitaTotal = receitaTotalPorProduto produtos
      putStrLn $ "Receita total gerada por todos os produtos: R$" ++ show receitaTotal
      putStrLn "-------------"
      exibirDashboards produtos clientes codigoProduto historicoCompras

    "08" -> do
      putStrLn "Total de receita gerada por categoria:"
      putStrLn "Digite a categoria para calcular a receita:"
      categoria <- getLine
      putStrLn "-------------"
      let receitaPorCategoria = receitaTotalPorCategoria produtos categoria
      putStrLn $ "Receita total gerada pela categoria '" ++ categoria ++ "': R$" ++ show receitaPorCategoria
      putStrLn "-------------"
      exibirDashboards produtos clientes codigoProduto historicoCompras

    "09" -> do
      putStrLn "Voltando ao menu do administrador."
      admController produtos clientes codigoProduto historicoCompras

    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      exibirDashboards produtos clientes codigoProduto historicoCompras
      