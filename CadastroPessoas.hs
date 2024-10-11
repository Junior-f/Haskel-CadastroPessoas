
type Cadastro = [Pessoa]

data Pessoa = Pessoa
  { nome :: String
   ,idade :: Int
   ,endereco :: Endereco
  } deriving (Show, Eq)

data Endereco = Endereco
  { rua :: String
   ,casa :: Int
   ,cidade :: String
  } deriving (Show, Eq)


-- funçao para ordenar as pessoas na lista em ordem alfabetica

ordenar :: Cadastro -> Cadastro
ordenar [] = []
ordenar (x : xs) = ordenar [e|e<-xs, nome e < nome x] ++ [x] ++ ordenar [e|e<-xs, nome e >= nome x]

-- funçao para inserir uma pessoa na lista

inserirPessoa :: Cadastro -> IO Cadastro
inserirPessoa cadastro = do
  putStrLn "\n-------INSERIR PESSOA-------"
  putStrLn "Digite o nome da pessoa"
  nome <- getLine
  putStrLn "Digite a idade da pessoa"
  idade <- getLine
  putStrLn "Digite nome da rua da pessoa"
  rua <- getLine
  putStrLn "Digite o numero da casa da pessoa"
  casa <- getLine
  putStrLn "Digite a cidade da pessoa"
  cidade <- getLine
  let pessoa = Pessoa nome (read idade :: Int) (Endereco rua (read casa :: Int) cidade)
  let pessoaOrdenada = ordenar(pessoa : cadastro)
  putStrLn " Pessoa Cadastrada com Sucesso"
  return pessoaOrdenada



-- função para localizar uma determinada pessoa pelo seu nome.

buscaPessoa :: Cadastro -> String -> [Pessoa]
buscaPessoa [] _ = []
buscaPessoa (x:xs) nomeP
  | nomeP == nome x = [x]
  | otherwise = buscaPessoa xs nomeP

localizarPessoa :: Cadastro -> IO ()
localizarPessoa [] = do putStrLn "\nCADASTRO VAZIO"
localizarPessoa cadastro = do
  putStrLn "\n-------LOCALIZAR PESSOA-------"
  putStrLn "Digite o nome"
  nome <- getLine
  let pessoa = buscaPessoa cadastro nome
  print pessoa




--função que recebe o cadastro de uma pessoa e atualiza suas informações na lista ou adiciona uma nova na ordem correta caso ela ainda não tenha sido cadastrada.

busca :: Cadastro -> Pessoa -> Cadastro
busca [] pessoa = [pessoa]
busca (x : xs) pessoa
  | nome pessoa == nome x = pessoa : xs
  | otherwise = x : busca xs pessoa

atualizarPessoa :: Cadastro -> Pessoa -> Cadastro
atualizarPessoa cadastro pessoa = busca cadastro pessoa

-- Ao atualizar a pessoa ele faz diferenca de maiusculas e minusculas ex: Junior /= junior, ex: Maria /= maria
--não sao a mesma pessoa, e na ordenaçao tambem as maiusculas irao ficar na frente de todas as minusculas
-- ex Yuri ira ficar na frente de ana.

atualizarDados :: Cadastro -> IO Cadastro
atualizarDados cadastro = do
  putStrLn "\n-------ATUALIZAR DADOS DA PESSOA-------"
  putStrLn "Digite o nome"
  nome <- getLine
  putStrLn "Digite a idade"
  idade <- getLine
  putStrLn "Digite nome da rua"
  rua <- getLine
  putStrLn "Digite o numero da casa"
  casa <- getLine
  putStrLn "Digite a cidade"
  cidade <- getLine
  let pessoa = Pessoa nome (read idade :: Int) (Endereco rua (read casa :: Int) cidade)
  let pessoaAtualizada = atualizarPessoa cadastro pessoa
  let pessoaOrdenada = ordenar pessoaAtualizada
  putStrLn "Dados da pessoa Atualizados"
  return pessoaOrdenada



--funçao para contar quantas pessoas moram em uma determinda cidade

contarPessoas :: Cadastro -> String -> Int
contarPessoas [] _ = 0
contarPessoas (x : xs) cidadeP
  | cidadeP == cidade (endereco x) = 1 + contarPessoas xs cidadeP
  | otherwise = contarPessoas xs cidadeP
  


relatorio :: Cadastro -> IO ()
relatorio [] = do putStrLn "\nCADASTRO VAZIO"
relatorio lista = do
  putStrLn "\n-------TOTAL PESSOAS CIDADE-------"
  putStrLn "Digite a cidade da pessoa"
  cidadeP <- getLine
  print (contarPessoas lista cidadeP)


--  função para apresentar a média de idade da população.
somaIdade :: Cadastro -> Int
somaIdade [] = 0
somaIdade (x : xs) = idade x + somaIdade xs

media :: Cadastro -> Float
media [] = 0
media lista = fromIntegral (somaIdade lista) / fromIntegral (length lista)

mostrarMedia :: Cadastro -> IO()
mostrarMedia [] = do putStrLn "\nCADASTRO VAZIO"
mostrarMedia lista = do
    putStrLn "\n-------MEDIA IDADE DA POPULAÇAO-------"
    print (media lista)


--funçao para mostar a lista por ordem alfabetica de pessoas cadastradas.
imprimir :: Cadastro -> IO ()
imprimir [] = do putStrLn "\nCADASTRO VAZIO"
imprimir lista = do
    putStrLn "\n-------PESSOAS CADASTRADAS-------"
    mapM_ print lista


menu :: Cadastro -> IO ()
menu dados = do
    putStrLn "\n-------MENU-------"
    putStrLn "1 - Inserir Dados da Pessoa"
    putStrLn "2 - Localizar Pessoa"
    putStrLn "3 - Atualizar Dados da Pessoa"
    putStrLn "4 - Relatorio Total Pessoas por Cidade"
    putStrLn "5 - Media Idade da Populacao"
    putStrLn "6 - Imprimir Pessoas Cadastradas"
    putStrLn "0 - Sair"
    op <- getLine
    case op of
        "1" -> do
            db <- inserirPessoa dados
            menu db
        "2" -> do
            localizarPessoa dados
            menu dados
        "3" -> do
            db <- atualizarDados dados
            menu db
        "4" -> do
            relatorio dados
            menu dados
        "5" -> do  
            mostrarMedia dados
            menu dados
        "6" -> do
            imprimir dados
            menu dados
        "0" -> do
            putStrLn "\n--------FIM--------"
        _ -> do
            putStrLn "Opcao invalida"
            menu dados

main :: IO ()
main = do
    menu []
    return ()