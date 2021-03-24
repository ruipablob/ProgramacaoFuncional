--1) Operador lógico OU (pré-fixo):
--A) Apresente 3 definições para o operador lógico OU, utilizando casamento de padrões.
-- (||) :: Bool->Bool->Bool
-- _ || True=True
-- True || _ =True
-- False || False=False

-- (||) :: Bool -> Bool -> Bool
-- True  || _ = True
-- False || y = y

-- (||) :: Bool -> Bool -> Bool
-- False || False = False
-- _     || _     = True

--B)  Apresente 2 definições para o operador lógico OU, utilizando expressões condicionais
--(no lugar de casamento de padrões).
-- (||) :: (Bool->Bool) -> Bool
-- (x,y)
--   |True  _ = True 
--   |False  False = False
--   | _  True = True


-- (||) :: Bool -> Bool -> Bool
--  (x y)  = if True || _ then True else if False || y then y

--2) Defina uma função que recebe dois pontos no espaço e retorna a distância entre eles.
--Considere que um ponto no espaço é representado por uma dupla de números (float) que
--correspondem às coordenadas do ponto
distanciap :: (Float,Float)->(Float,Float)->Float
distanciap (x,y)(x1,y1) = sqrt(((x1-x)^2)+((y1-y)^2))

--3) Dado um valor inteiro, escreva a função recursiva fatorial. Obs: Fazer uma
--definição usando guardas e outra com casamento de padrões.
fatorialg x
  | x ==  0 = 1
  |otherwise =  x*fatorialg(x-1)

fatorialcp :: Int -> Int
fatorialcp 0 = 1
fatorialcp x = x * fatorialcp(x-1)

--4) Dado um número inteiro n, escreva a função recursiva fibo que retorna o n-ésimo
--termo da sequência de Fibonacci a seguir, sendo os casos base F0 = 0 e F1 = 1. Utilize a
--definição recursiva vista em sala: fibo(n) = fibo(n-2) + fibo(n-1).
--fibo(n) = fibo(n-2) + fibo(n-1).
fibo :: Int->Int
fibo 0 = 0
fibo 1 = 1
fibo x = fibo(x-2) + fibo(x-1)

--5) Dado um número inteiro n, escreva a função recursiva n_tri, que retorna o n-ésimo
--termo da sequência de números triangulares, dada a seguir.
n_tri :: Int -> Int
n_tri 0 = 0
n_tri 1 = 1
n_tri x = x + n_tri(x-1)

--6) Escreva a função potencia2, que calcula a potência de 2 elevada a um expoente n
--de forma recursiva: 2
--n = 2n-1 * 2.
potencia2 :: Int -> Int
potencia2 0 = 1
potencia2 x = 2 * potencia2(x-1)

--7) a) Escreva a função recursiva prodIntervalo: dados dois inteiros m e n, onde m<n,
--retorna o produto: m*(m+1)*...(n-1)*n.
prodIntervalo :: Int -> Int -> Int
prodIntervalo m n
 |m > n = 1
 |otherwise = m * prodIntervalo(m+1) n

--B)  b) Reescreva a função fatorial usando a função prodIntervalo.
fatorialPInter :: Int -> Int
fatorialPInter 0 = 1
fatorialPInter x = prodIntervalo 1 x

--Ex11)Defina de forma recursiva as funções resto_div e div_inteira, que retornam o
--sucessivas de n a partir de m.
--Ex: m=20 e n=3: 20-3=17, 17-3=14, 14-3=11, 11-3=8, 8-3=5, 5-3=2.
--Como 2<3: resto=2 e quociente=6.
resto_div :: Int -> Int -> Int
resto_div m n 
  |m<n=m
  |otherwise = resto_div (m - n)n

div_inteira :: Int -> Int -> Int
div_inteira m n
  |m < n = 0
  |otherwise = 1 + div_inteira (m - n) n

--9) Implemente a função mdc, usando a definição recursiva vista em sala:
--mdc(m,n) = m, se n = 0
--mdc(m,n) = mdc(n, k), se n > 0, sendo k = m mod n
--Obs: Fazer uma definição usando guardas e outra com casamento de padrões
mdc :: (Int,Int) -> Int
mdc (m,0) = m
mdc (m,n) = mdc (n, (mod m n))

mdcg :: (Int,Int) -> Int
mdcg (m,n)
 | n == 0 = m
 | otherwise = mdcg (n, (mod m n))

--10)10) Implemente a função binomial usando a definição recursiva vista em sala:
-- binomial (n,k) = 1, se k = 0
-- binomial (n,k) = 1, se k = n
-- binomial (n,k) = binomial (n-1,k) + binomial (n-1,k-1), se 0 < k < n
-- Observe que binomial (n,k) não é definido se k>n.
-- Obs: Fazer uma definição usando guardas e outra com casamento de padrões.
binomialg :: (Int,Int) -> Int
binomialg (n,0) = 1
binomialg (n,k)
 | k == 0 = 1
 | k == n = 1
 | otherwise = binomialg (n-1,k) + binomialg (n-1,k-1)

binomial :: (Int,Int) -> Int
binomial (n,0) = 1
binomial (n,k) = if (k == n) then 1 else binomial (n-1,k) + binomial (n-1,k-1)

--11)
--A)Defina um par na sequência de Fibonacci como (n,n+1).
--Exemplos: (1,1), (3,5), (55,89), (233,377)
fibo2 :: Int -> (Int,Int) 
fibo2 n = (fibo n,fibo (n+1))

--B) Dois pares consecutivos na sequência podem ser considerados como um passo:
--(x,y) => (y, x+y). Exemplos: (1,1) => (1,2); (3,5) => (5,8); (55,89) => (89, 144)
fibo3 :: Int -> Int -> ((Int,Int),(Int,Int))
fibo3 n m = ((fibo n,fibo m),(fibo m, fibo(n+m)))

--C)c) A partir do par inicial (1,1), podemos definir o enésimo par, como a aplicação
-- consecutiva de n passos:
-- (1,1) => (1,2) => (2,3) => (3,5) => (5,8) => (8,13) => (13,21) => (21,34) => (34,55) =>...
fibo4 :: Int -> (Int,Int)
fibo4 n = fibo2 n

--D)d) O n-ésimo termo (para n>0) é o primeiro elemento do enésimo par.
--Ex: quarto par: (3,5) e quarto termo: 3 e décimo par: (55,89) e décimo termo: 55
fibo5 :: Int -> ((Int,Int),Int)
fibo5 n = ((fibo4 n),fibo n)