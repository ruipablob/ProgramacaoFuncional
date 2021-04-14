--Nomes:Patrícia Fernandes Dornelas, Rui Pablo de Brito Ferreira

-- 1) Escreva a função analisa_raizes que, dados os 3 coeficientes a, b e c de uma
-- equação quadrática ax2 +bx + c = 0, realiza a análise das raízes da equação. A equação
-- é dita degenerada, se o coeficiente do termo quadrático a for igual a zero. Por outro lado,
-- uma equação não degenerada possui o número de raízes reais de acordo com as regras:
-- (1) a equação possui duas raízes reais, se b2 > 4*a*c; (2) a equação possui uma raiz real,
-- se b2 = 4*a*c; (3) a equação não possui raízes reais, se b2 < 4*a*c. A análise de saída
-- deve ser uma das 4 opções a seguir: “1-possui duas raízes reais”, “2-possui uma raiz
-- real”, “3-nenhuma raiz real” ou “4-equação degenerada”.
analisa_raizes :: Int -> Int -> Int -> String
analisa_raizes a b c
  |a==0 = "4 -equação degenerada"
  |b^2 > 4*a*c = "1-possui duas raízes reais"
  |b^2 < 4*a*c = "3-não possui raízes reais"
  |otherwise =  "2-possui uma raiz real"

-- 2) Escreva a função equacao que recebe três valores reais a, b, c. Se a for diferente de
-- 0, a função retorna uma tupla com as duas raízes da equação de segundo grau ax2 + bx
-- + c = 0. Se Se a for igual a 0, a função retorna uma tupla, sendo o primeiro elemento a
-- solução da equação de primeiro grau bx + c = 0 e o segundo elemento o próprio a.
equacao :: Float-> Float -> Float ->(Float,Float)
equacao a b c 
    |a /= 0 = (((-b)+sqrt((b*b)-4*(a*c)))/(2*a),((-b)-sqrt((b*b)-4*(a*c)))/(2*a))
    |a == 0 = ((-c)/b,a)

-- 3) Considere que o preço de uma passagem de ônibus intermunicipal pode variar
-- dependendo da idade do passageiro. Crianças até 10 anos pagam 40% e bebês (abaixo
-- de 2 anos) pagam apenas 15%. Pessoas com 70 anos ou mais pagam apenas 50% do
-- preço total. Os demais passageiros, pagam a tarifa normal (100%). Faça uma função que
-- tenha como entrada: o valor total da passagem, a data atual e a data de nascimento do
-- passageiro. Como saída, a função retorna o valor a ser pago. (Obs.: na solução, deve ser
-- definido o tipo data para representar a tupla (d,m,a)).
type Data = (Int,Int,Int)
passagem :: Float ->Data ->Data ->Float
passagem v (d,m,a) (d1,m1,a1)
    |a-a1 > 2 && a-a1 <= 10 = (40/100)*v
    |a-a1 > 0 && a-a1 <= 2 = (15/100)*v
    |a-a1 <= 70 = (50/100)*v
    |otherwise =  v

-- 4) Construa funções que gerem as seguintes listas, utilizando-se lista por compressão.
-- Todas as funções devem utilizar a lista de inteiros de 1 a 20 em pelo menos um dos
-- geradores. Apresentar o código da função e o resultado da lista gerada.
-- a) gera1: gerar a lista de inteiros, contendo o cubo de todos
-- os pares entre 3 e 11.
gera1 = [x^3 | x <- [3..11], even x]

-- b) gera2: gerar a lista de duplas formadas tendo o primeiro
-- elemento menor ou igual a 5 e o segundo elemento no intervalo
-- fechado entre o valor do primeiro elemento e o seu triplo.
gera2 = [(x,y) | x <- [5,4..1], y<-[x+1..(x*3)-1]]

-- c) gera3: a partir de uma lista l1=[15,16], gerar a lista com
-- todos os elementos dentro do intervalo fechado definido entre
-- 1 e cada elemento de l1 (Obs.: pode ter elemento repetido na
-- lista final).
l1 = [15,16]
gera3 = [x | y<- l1 ,x<-[1..y]]

-- d) gera4: gerar uma lista de duplas, onde cada dupla são 2
-- números consecutivos de 1 a 10, sendo o primeiro elemento par
-- (Ex: (2,3) e (4,5))
gera4 = [(x,x+1) | x <- [1..10], even x]

-- e) gera5: a partir da lista de duplas geradas no item d,
-- gerar a lista onde cada elemento corresponde à soma dos
-- elementos da dupla.
gera5 = [x  | x <- gera4]

-- 5) a) Escreva uma função (usando compreensão de listas) que calcula a quantidade de
-- números que são positivos e múltiplos de 3 (ao mesmo tempo) de uma lista de inteiros:
-- > contaNegM2 [1,-3,-4,3,4,-5,-8,-7,9]
-- 2
contaNegM2 :: [Int] -> Int
contaNegM2 lista = length [x | x<-lista, x>0 && mod x 3 ==0]

-- b) Escreva uma função (usando compreensão de listas) que extrai números que são
-- positivos e múltiplos de 3 (ao mesmo tempo) de uma lista de inteiros e os retorna em uma
-- nova lista:
-- >listaNegM2 [1,-3,-4,3,4,-5,-8,-7,9]
-- [3,9]
listaNegM2 :: [Int] -> [Int]
listaNegM2 lista = [x | x<-lista, x>0 && mod x 3 == 0]

-- 6) Escreva a função primos a seguir que recebe dois valores inteiros x,y e retorna todos
-- os números primos que se encontram entre x e y. Obs: construir uma segunda função
-- fatores que retorna todos os divisores de um número inteiro e utilizá-la na elaboração
-- da função primos.
-- > primos 10 50
-- [11,13,17,19,23,29,31,37,41,43,47]
fatores :: Int -> [Int]
fatores n = [x | x<-[1..n], mod n x == 0]

primos :: Int -> Int -> [Int]
primos x y = [z | z<-[x..y], fatores z == [1,z]]

-- 7) Construa a função mmc a seguir que calcula o valor do mínimo múltiplo comum de três
-- números inteiros.
-- > mmc 2 3 4
-- 12
mdc::(Int,Int) -> Int
mdc (x,y)
 |y == 0 = x           
 |otherwise = mdc (y, (mod x y))

auxmmc::Int->Int->Int
auxmmc x y =  div (x*y) (mdc (x,y))

mmc::Int->Int->Int->Int
mmc x y z = auxmmc x (auxmmc y z)

-- 8) Escreva uma função que calcula a série a seguir, dados um número real x e o número
-- de termos a serem calculados n. Obs: se preciso, use a função fromIntegral para
-- converter n de Inteiro para Float.
serie :: Float -> Int -> Float
serie x 0 = 0
serie x n = if even n == True then (x /fromIntegral(n)) + serie x (n-1) else (fromIntegral(n) /(x)) + serie x (n-1)


-- 9) Escreva a função fizzbuzz a seguir que recebe um inteiro n e retorna uma lista de
-- strings. Para cada inteiro i entre 1 e n, a lista será composta da seguinte forma.
-- • Se i é divisível por 2, escreva “Fizz”.
-- • Se i é divisível por 3, escreva “Buzz”.
-- • Se i é divisível por ambos 2 e 3, escreva “FizzBuzz”.
-- • Caso contrário, diga “No”.
-- Exemplo:
-- > fizzbuzz 10
-- ["No","Fizz","Buzz","Fizz","No","FizzBuzz","No","Fizz","Buzz","Fizz"]
auxfiz :: Int -> String
auxfiz i 
  |(mod i 2 == 0 && mod i 3 == 0) = "FizzBuzz"
  |mod i 2 == 0 = "Fizz"
  |mod i 3 == 0 = "Buzz"
  |otherwise = "No"

fizzbuzz :: Int -> [String]
fizzbuzz n = [auxfiz x | x<-[1..n]]

-- 10) Usando lista por compreensão, escreva a função seleciona_multiplos que recebe um
-- lista de inteiros e um inteiro n e retorna uma nova lista com todos os números presentes na lista
-- de entrada que são múltiplos de n. Ex:
-- > sel_multiplos 3 [2,6,4,3,1,5,9]
-- [6,3,9]
-- > sel_multiplos 2 [2,6,4,3,1,5,9]
-- [2,6,4]
sel_multiplos :: [Int] -> Int -> [Int]
sel_multiplos x n = [z | z <- x, mod z n == 0]

-- 11) Escreva a função unica_ocorrencia a seguir que recebe um elemento e uma lista
-- e verifica se existe uma única ocorrência do elemento na lista .
-- > unica_ocorrencia 2 [1,2,3,2]
-- False
-- > unica_ocorrencia 2 [3,1]
--False
unica_ocorrencia :: Int -> [Int] -> Bool
unica_ocorrencia _ [] = False 
unica_ocorrencia x (y:ys)
   |x == y && elem x ys == False = True
   |x == y && elem x ys == True = False
   |otherwise = unica_ocorrencia x ys

-- 12) Crie uma função que intercala os elementos de duas listas de qualquer tamanho
-- numa nova lista. Obs: as listas de entrada devem ser do mesmo tipo mas podem ter
-- tamanhos diferentes. Caso sejam diferentes, os elementos excedentes da lista maior
-- devem complementar a lista de saída
-- > intercala [1,2,3,4] [100,200]
-- [1,100,2,200,3,4]
-- > intercala [1,2] [100,200,300]
-- [1,100,2,200,300]
intercala :: [Int] -> [Int] -> [Int]
intercala x [] = x
intercala [] y = y
intercala (x:xs) (y:ys) = x:y:intercala xs ys

-- 13) Construa a função zipar que recebe duas listas de mesmo tipo e elabora uma terceira lista
-- de sublistas de tamanho 2 formadas por um elemento da primeira lista e outro da segunda lista.
-- Assim, a primeira sublista da lista de saída é formada com o primeiro elemento de cada lista, a
-- segunda sublista é formada com o segundo elemento de cada uma, e assim por diante. Se listas
-- de entrada tiverem tamanhos diferentes, a lista de saída é truncada para o tamanho da menor lista
-- de entrada. Obs: não é permitido usar funções de manipulação de listas da biblioteca PRELUDE
-- padrão do Haskell. Ex:
-- > zipar [1,2,3,4] [5,6,7,8]
-- [[1,5],[2,6],[3,7],[4,8]]
-- > zipar [1,2,3] [4,5]
-- [[1,4],[2,5]]
zipar :: [Int] -> [Int] -> [[Int]]
zipar x [] = []
zipar [] y = []
zipar (x:xs) (y:ys) =  [x,y] : zipar xs ys

-- 14) Defina novos tipos para representar os dados contidos numa agenda pessoal. Para
-- cada contato, armazene as informações: nome, endereço, telefone, e-mail. Em seguida,
-- crie uma função para recuperar o nome de um contato, a partir do email. Caso o número
-- não seja encontrado, retornar a mensagem “Email desconhecido”.
type Contato = (String, String, String, String)
type Agenda = [Contato]
contato :: Agenda
contato = [("Patricia", "Uma rua ai", "111", "patriciafdornelas@"),
   ("Maria", "Duas ruas ai", "222", "mdemaria@"),
   ("Jose", "Tres ruas ai", "333", "jdejose@")]

agendaPessoal :: Agenda -> String -> String
agendaPessoal [] _ = "Email desconhecido"
agendaPessoal ((nome,_,_,email):xs) email1 
   |email1 == email = nome 
   |email1 /= email = agendaPessoal xs email
   |otherwise = "Email desconhecido"

-- 15) Seja o tipo Pessoa e a lista de pessoas a seguir.
-- O tipo pessoa é uma tupla que inclui nome, altura, idade e estado civil (‘c’ ou ‘s’).
type Pessoa = (String, Float, Int, Char)
pessoas :: [Pessoa]
pessoas = [ ("Rosa", 1.66, 27,'F'),("João", 1.85, 26, 'C'),("Maria", 1.55, 62, 'S'),("Jose", 1.78, 42, 'C'),("Paulo", 1.93, 25, 'S'),("Clara", 1.70, 33, 'C'),("Bob", 1.45, 21, 'C'),("Rosana", 1.58,39, 'S'),("Daniel", 1.74, 72, 'S'),("Jocileide", 1.69, 18, 'S') ]

-- Escreva funções que, dada a lista pessoas, retornem:
-- • A altura média entre todas as pessoas.
mediaAlturaContador :: [Pessoa] -> Float
mediaAlturaContador [] = 0
mediaAlturaContador ((a,altura,b,c):xs) = 1 + mediaAlturaContador xs
mediaAlturaSoma :: [Pessoa] -> Float
mediaAlturaSoma [] = 0
mediaAlturaSoma ((a,altura,b,c):xs) = altura + mediaAlturaSoma xs
mediaAltura :: [Pessoa] -> Float
mediaAltura [] = 0
mediaAltura ((a,altura,b,c):xs) = mediaAlturaSoma ((a,altura,b,c):xs) / mediaAlturaContador ((a,altura,b,c):xs)

-- • A idade da pessoa mais nova.
idadePNovaaux :: [Pessoa] -> [Int]
idadePNovaaux [] = []
idadePNovaaux ((_,_,idade,_):xs) = idade:idadePNovaaux xs
idadePNova :: [Pessoa] -> Int
idadePNova ((a,s,idade,d):xs) = minimum (idadePNovaaux ((a,s,idade,d):xs))

-- • O nome e o estado civil da pessoa mais velha.
nomeEstaMaisVelhaaux :: [Pessoa] -> Int
nomeEstaMaisVelhaaux ((nome,tamanho,idade,estcivil):xs) = maximum (idadePNovaaux ((nome,tamanho,idade,estcivil):xs))
nomeEstaMaisVelha :: [Pessoa] -> (String,Char)
nomeEstaMaisVelha ((nome,tamanho,idade,estcivil):xs)
   |idade == nomeEstaMaisVelhaaux ((nome,tamanho,idade,estcivil):xs) = (nome,estcivil)
   |otherwise = nomeEstaMaisVelha xs

-- • Todos os dados de cada pessoa com 50 anos ou mais.
mais50 :: [Pessoa] -> [(String, Float, Int, Char)]
mais50 [] = []
mais50 ((nome,tamanho,idade,estcivil):xs)
   |idade >= 50 = [(nome,tamanho,idade,estcivil)] ++ mais50 xs
   |otherwise = mais50 xs

-- • O número de pessoas casadas com idade superior a i (ex: i = 35).
casadasMais35aux :: [Pessoa] -> [Int]
casadasMais35aux ((nome,tamanho,idade,estcivil):xs) = idadePNovaaux ((nome,tamanho,idade,estcivil):xs)

casadasMais35 :: [Pessoa] -> Int
casadasMais35 [] = 0
casadasMais35 ((nome,tamanho,idade,estcivil):xs)
   |idade >= 35 && estcivil == 'C' = 1 + casadasMais35 xs
   |otherwise = casadasMais35 xs

-- 16) Escreva a função insere_ord a seguir, que recebe uma lista polimórfica
-- ordenada de elementos (critério de ordenação crescente) e um novo elemento x (do
-- mesmo tipo da lista) e retorna a nova lista com o novo elemento inserido
-- > insere_ord 5 [1,4,7,11]
-- [1,4,5,7,11]
-- > insere_ord 'g' "abcjkl"
-- "abcgjkl"
insereord :: Ord a => [a] -> a -> [a]
insereord [] x = [x]
insereord (x:xs) y
   |y < x = y:x:xs
   |otherwise = x:insereord xs y

-- 17) Escreva a função reverte a seguir que recebe uma lista polimórfica e retorna uma
-- lista com seus elementos ao contrário.
-- > reverte [1,2,3,4]
-- [4,3,2,1]
-- > reverte "abcd"
-- "dcba"
reverte :: Eq a => [a] -> [a]
reverte [] = []
reverte [x] = [x]
reverte (x:xs) = reverte xs ++ [x]

-- 18) Escreva a função elimina_repet a seguir que recebe uma lista polimórfica e
-- retorna uma lista sem elementos repetidos.
-- > elimina_repet [3,3,2,9,1,7,2,5,9,7]
-- [3,2,9,1,7,5]
-- > elimina_repet "mississippi"
-- "misp"
eliminaRepetidosaux :: Eq a => [a] -> a -> [a]
eliminaRepetidosaux [] a = []
eliminaRepetidosaux (x:xs) z
   |x == z = eliminaRepetidosaux xs z
   |otherwise = x:eliminaRepetidosaux xs z

eliminaRepetidos :: Eq a => [a] -> [a]
eliminaRepetidos [] = []
eliminaRepetidos (x:xs) = x:eliminaRepetidos(eliminaRepetidosaux xs x)

-- 19) Escreva a função notasTroco a seguir usando compreensão de listas que calcula
-- todas as combinações de notas para devolver o troco durante um pagamento, a partir de
-- uma lista com os valores das notas disponíveis (definido no arquivo .hs) e o valor do troco
-- x (argumento da função). Ex:
-- Considere disponiveis = [1,2,5,10,20,50,100]
-- > notasTroco 4
-- [[1,1,1,1],[1,1,2],[1,2,1],[2,1,1],[2,2]]
-- > notasTroco 7
-- [[1,1,1,1,1,1,1],[1,1,1,1,1,2],[1,1,1,1,2,1],[1,1,1,2,1,1],
-- [1,1,1,2,2],[1,1,2,1,1,1],[1,1,2,1,2],[1,1,2,2,1],[1,1,5],
-- [1,2,1,1,1,1],[1,2,1,1,2],[1,2,1,2,1],[1,2,2,1,1],[1,2,2,2],
-- [1,5,1],[2,1,1,1,1,1],[2,1,1,1,2],[2,1,1,2,1],[2,1,2,1,1],
-- [2,1,2,2],[2,2,1,1,1],[2,2,1,2],[2,2,2,1],[2,5],[5,1,1],[5,2]]
disponiveis = [1,2,5,10,20,50,100]

notasTroco :: Int -> [[Int]]
notasTroco 0 = [[]]
notasTroco x = [lista:listas | lista <- disponiveis, x >= lista, listas <- notasTroco (x-1)]
