-- 1) Refaça as seguintes funções dos roteiros 2, 3 e 4, utilizando o comando
-- “where” para definições locais (incluindo funções auxiliares que são
-- necessárias na solução da função principal):
-- (WHERE)
type Data = (Int,Int,Int)

bissexto :: Int -> String
bissexto x 
  |y = "Nao eh bissexto"
  |otherwise = "Eh bissexto"
     where y = mod x 4 /= 0 && (mod x 100 == 0 || mod x 400 /= 0)    
-- a) Escreva a função valida que indica se uma data é válida ou não.
-- (WHERE)
valida :: Data -> Bool
valida(d,m,a)
  |x = True
  |y = True
  |z = True
  |otherwise = False
     where
       x = (m == 2 && (d>0 && d <= 28) && a>0) || (m == 2 && (d>0 && d <= 29) && bissexto a == "Eh bissexto" )
       y = (m==4 || m==6 || m==9 || m==11) && (d>0 && d < 31) 
       z = (m==1 || m==3 || m==5 || m==7 || m==8 || m==10 || m==12) && (d>0 && d<=31)
-- b) Escreva a função bissextos a seguir que recebe uma lista de inteiros e
-- retorna uma lista com os valores que representam anos bissextos.
-- (WHERE)
bissextos :: [Int] -> [Int]
bissextos x = y
   where y = [xs | xs<-x, bissexto xs == "eh bissexto"]
-- c) Escreva a função atrasados que recebe um parâmetro do tipo
-- Emprestimos e a Data atual, e retorna uma lista com todos os
-- empréstimos atrasados.
-- (WHERE)
type Livro = (String, String, String, String, Int)
type Aluno = (String, String, String, String)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]
bdEmprestimo::Emprestimos
bdEmprestimo =
 [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
 ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
 ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

precede :: Data -> Data -> Bool
precede (d,m,a) (d1, m1, a1)
  | x = True
  | y = True
  | z = True
  | otherwise = False 
     where 
       x = a < a1
       y = a == a1  && m < m1
       z = a == a1 && m == m1 && d < d1

verifica::Emprestimo->Data->Bool
verifica (cdl, cda, (d,m,a),(d1,m1,a1),sit) (dh,mh,ah)
 |x = True
 |y = True
 |z = True
 |otherwise = False
     where
       x = (sit == "fechado")
       y = precede (dh,mh,ah) (d1,m1,a1) && (sit == "aberto")
       z = ((dh,mh,ah) == (d1,m1,a1)) && (sit == "aberto")

atrasados::Emprestimos->Data->Emprestimos
atrasados x d = z
     where z = [y | y<-x, not(verifica y d)]
-- d) Faça uma segunda definição da função recursiva fibo2 que retorna
-- o n-ésimo termo da sequência de Fibonacci utilizando recursividade
-- e os conceitos a seguir (use a função passo(x,y)).
-- (WHERE)
fibo :: Int->Int
fibo 0 = 0
fibo 1 = 1
fibo x = y
    where y = fibo(x-2) + fibo(x-1)

fibo2 :: Int -> (Int,Int) 
fibo2 n = x
    where x = (fibo n,fibo (n+1))
-- e) Escreva a função fatorial usando a função prodIntervalo.
-- (WHERE)
prodIntervalo :: Int -> Int -> Int
prodIntervalo m n
 |x = 1
 |otherwise = m * y
     where 
       x = m > n
       y = prodIntervalo(m+1) n

fatorialPInter :: Int -> Int
fatorialPInter 0 = 1
fatorialPInter x = z
    where z = prodIntervalo 1 x

-- 2) Refaça as funções do exercício 1, utilizando o comando “let” para
-- definições locais (incluindo funções auxiliares que são necessárias na
-- solução da função principal). Repetir para os itens “a” a “e”.

--A)
bissextolet :: Int -> String
bissextolet x = let
                  y = mod x 4 /= 0 && (mod x 100 == 0 || mod x 400 /= 0)
                  in
                  if y then "Nao eh bissexto" else "Eh bissexto"
  
validalet :: Data -> Bool
validalet (d,m,a) = let
                      x = (m == 2 && (d>0 && d <= 28) && a>0) || (m == 2 && (d>0 && d <= 29) && bissexto a == "Eh bissexto" )
                      y = (m==4 || m==6 || m==9 || m==11) && (d>0 && d < 31) 
                      z = (m==1 || m==3 || m==5 || m==7 || m==8 || m==10 || m==12) && (d>0 && d<=31)
                    in
                    if x then True else if y then True else if z then True else False
--B)
bissextoslet :: [Int] -> [Int]
bissextoslet x = let y = [xs | xs<-x, bissextolet xs == "eh bissexto"] 
              in y    

--C)
verificalet::Emprestimo->Data->Bool
verificalet (cdl, cda, (d,m,a),(d1,m1,a1),sit) (dh,mh,ah) = let
                                                             x = (sit == "fechado")
                                                             y = precede (dh,mh,ah) (d1,m1,a1) && (sit == "aberto")
                                                             z = ((dh,mh,ah) == (d1,m1,a1)) && (sit == "aberto")
                                                            in
                                                             if x then True else if y then True else if z then True else False

atrasadoslet::Emprestimos->Data->Emprestimos
atrasadoslet x d = let
                     y = [y | y<-x, not(verificalet y d)]
                   in y

--D)
fibolet :: Int->Int
fibolet 0 = 0
fibolet 1 = 1
fibolet x = let
              y = fibolet(x-2) + fibolet(x-1)
            in 
              y

fibo2let :: Int -> (Int,Int) 
fibo2let n = let
              y = (fibolet n,fibolet (n+1)) 
             in 
              y

--3) Aplicar Beta-redução nas expressões lambda a seguir:
{-
1) (λ x.2*x + 1) 3
   (λ x. 2*3 + 1)
   (λ x. 6 + 1)
     (6+1) = 7

2) (λ xy.x-y) 5 7
   (λ xy. 5 - y) 7
   (λ xy. 5 - 7) 
     (5-7) = 2

3) (λ yx.x-y) 5 7
   (λ yx.x-5) 7
   (λ yx. 7-5)
     (7-5) = 2

4) (λ xy. x-y) (λz.z/2)
   (λ xy. x-y) (z/2)
   (λ xy. (z/2)-y)

5) (λ xy.x-y) ((λz.z/2)6)1
   (λ xy.x-y) ((λz.6/2)1
   (λ xy.x-y) ((3)1
   (λ xy.x-y) 3 1
   (λ xy.3-y) 1
   (λ xy.3-1)
   (3-1) = 2

6) (λ x.λ y. - x y) 9 4 
   (λ x.λ y. - 9 y) 4 
   (λ x.λ y. - 9 4)
   (9-4) = 5

7)(λ x.xx) (λ y.y)
  (λ x.xx) y
  (λ x.yy)

4) Abra o ambiente interativo GHCi e avalie as seguintes expressões.

(\x -> x + 3) 5                 = 8
(\x -> \y -> x * y + 5) 3 4     = 17
(\(x,y) -> x * y^2) (3,4)       = 48
(\(x,y,_) -> x * y^2) (3,4,2)   = 48
(\xs -> zip xs [1,2,3]) [4,5,6] = [(4,1),(5,2),(6,3)]

5) Codifique as seguintes expressões do cálculo lambda em Haskell e avalie as
mesmas no GHCi:
a) (λx λy. y)((λz. z)(λz. z))(λw. w) 5
(\x -> \y -> y)((\z->z)(\z->z))(\w->w) 5
> 5

b) ((λf. (λx. f(f x))) (λy. (y * y))) 3
((\f->(\x->f(f x)))(\y->(y*y))) 3
>81

c) ((λf. (λx. f(f x)))(λy.(+ y y))) 5
((\f->(\x->f(f x)))(\y->(y+y))) 5
>20

d) ((λx. (λy. + x y) 5) ((λy. - y 3) 7))
((\x->(\y->x + y ) 5)((\y-> y - 3)7))
>9

e) (((λf. (λx. f(f(f x)))) (λy. (y * y))) 2)
((\f->(\x->f(f(f x))))(\y->(y *y))) 2
>256

f) (λx. λy. + x ((λx. - x 3) y)) 5 6
(\x-> \y-> x + ((\x-> x - 3)y)) 5 6
>8
