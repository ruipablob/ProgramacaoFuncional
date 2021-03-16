--1) Reescreva as funções do último roteiro (dobro, quadruplicar, hipotenusa, distância),
--definindo a prototipação de tipos de cada função. Obs: se você já fez isso no primeiro
--roteiro, envie novamente para o exercício ficar completo.

dobro :: Int -> Int
dobro x = x * 2

quadruplo :: Int -> Int
quadruplo x = dobro(dobro x)

hipotenusa :: Float -> Float -> Float
hipotenusa a b = sqrt(a^2+b^2)

distancia :: Float -> Float -> Float -> Float -> Float
distancia x1 y1 x2 y2 = sqrt((x2 - x1)^2+(y2 - y1)^2)

{-
2) Abra o ambiente interativo GHCi e avalie as seguintes expressões.
fst (2,5) = 2
snd (5, "Bom dia") = "Bom dia"
fst(snd ("Ola", (1,2))) = 1
snd(fst ("Ola", (1,2))) = 
	<interactive>:14:5: error:
    * Couldn't match expected type `(a0, b)' with actual type `[Char]'
    * In the first argument of `snd', namely `(fst ("Ola", (1, 2)))'
      In the expression: snd (fst ("Ola", (1, 2)))
      In an equation for `it': it = snd (fst ("Ola", (1, 2)))
    * Relevant bindings include it :: b (bound at <interactive>:14:1)
(1,1) == (1,1) = True
(1,1) /= (1,1) = False
(1,1) < (1,2) = True
(2,1) < (1,2) = False
(1,2,3) < (1,2) = 
	<interactive>:19:11: error:
    * Couldn't match expected type `(a0, b0, c0)'
                  with actual type `(a1, b1)'
    * In the second argument of `(<)', namely `(1, 2)'
      In the expression: (1, 2, 3) < (1, 2)
      In an equation for `it': it = (1, 2, 3) < (1, 2)
"azul" < "verde" = True
"azul" < "amarelo" = False
(1,2,3) == (,,) 1 2 3 = True
-}

--3) Dado um valor monetário em reais, escreva uma função conversao que retorna uma tupla-3
--com o valor em Real, Dolar e Euro, sendo que 1 real = 3,96 dólares = 4,45 euros
conversao :: Float -> (Float,Float,Float)
conversao x = (x, x*5.61, x*6.69)

--4) Implemente a função bissexto que, dado um ano (inteiro), indique se ele é bissexto ou não.
bissexto :: Int -> String
bissexto x 
  |mod x 4 /= 0 && (mod x 100 == 0 || mod x 400 /= 0)  = "Nao eh bissexto"
  |otherwise = "Eh bissexto"
--5) Defina o tipo Data dado a seguir. Escreva a função bissexto2 que recebe uma data e indique
--se ela pertence a um ano bissexto ou não.
type Data = (Int, Int, Int)

bissexto2::Data -> Bool
bissexto2 (d,m,a) = if bissexto a == "Nao eh bissexto" then False else True
--6) Escreva a função valida que indica se uma data é válida ou não.
valida :: Data -> Bool
valida(d,m,a)
  |(m == 2 && (d>0 && d <= 28) && a>0) || (m == 2 && (d>0 && d <= 29) && bissexto a == "Eh bissexto" )  = True
  |(m==4 || m==6 || m==9 || m==11) && (d>0 && d < 31) = True
  |(m==1 || m==3 || m==5 || m==7 || m==8 || m==10 || m==12) && (d>0 && d<=31) = True
  |otherwise = False
--7) Escreva a função precede que recebe 2 datas e indica se a 1a data é anterior à 2a.
precede::Data -> Data -> Bool
precede (d,m,a)(d1,m1,a1)
  |valida(d,m,a) == True && valida(d1,m1,a1) == True && (d < d1 && m < m1 && a <= a1) = True
  |valida(d,m,a) == True && valida(d1,m1,a1) == True && (m < m1 && a <= a1) = True
  |valida(d,m,a) == True && valida(d1,m1,a1) == True && (a <= a1) = True
  |otherwise = False
-- 8) Implemente as estruturas de dados (tuplas) para um sistema de gerenciamento de bibliotecas e
-- depois as defina como tipos. O sistema tem 3 estruturas básicas:
-- Livro: composto por código do livro, título do livro, autor, editora e ano de publicação.
-- Aluno: composto por código do aluno, nome, e-mail e telefone.
-- Empréstimo: composto por código do livro, código do aluno, data de empréstimo, data de
-- devolução e situação. Obs: utilize a estrutura/tipo auxiliar data do exercício 4
type Livro = (String, String, String, String, Int)
type Aluno = (String, String, String, String)
type Emprestimo = (String, String, Data, Data, String)
-- 9) Seja o tipo Emprestimo e o exemplo dado a seguir, composto por código do livro, código do
-- aluno, data de empréstimo, data de devolução e situação. Escreva uma função que verifica se um
-- empréstimo está em dia, dado um empréstimo e a data de hoje.
-- type Emprestimo = (String, String, Data, Data, String)
-- e1::Emprestimo
-- e1 = ("H123C9","BSI200945",(12,9,2009),(20,9,2009),"aberto")
e1::Emprestimo
e1 = ("H123C9","BSI200945",(12,9,2009),(20,9,2009),"aberto")

verifica :: Emprestimo -> Data -> String
verifica (cl,ca,(d,m,a),(d1,m1,a1),s) (dh,mh,ah)
  |precede(dh,mh,ah)(d1,m1,a1) == True = "Em dia"
  |otherwise = "Atrasado"