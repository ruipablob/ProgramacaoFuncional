--1) Abra o ambiente interativo GHC no repl.it (https://repl.it/languages/haskell) e avalie as seguintes expressões.
{-
1 + 2 * 3       = 7
5 ^ 3           = 125
5 ** 3          = 125.0
5 / 3           = 1.6666666666666667
div 5 3         = 1
mod 5 3         = 2 
5 < 3           = False
mod 5 3 < 2     = False
mod 5 3 == 2    = True
sqrt 81         = 9.0
logBase 2 1024  = 10.0
floor 5.7       = 5
ceiling 5.7     = 6 
abs (-5)        = -5
min 6 7         = 6
max 6 7         = 7
sin (pi/2)      = 1.0
sum [1..5]      = 15
not True        = False
True && False   = False
-}

--2) Escreva uma função para calcular o dobro de um número.
dobro x = x * 2
--3) Escreva uma função para quadruplicar um número usando a função dobro definida no item anterior
quadruplo x = dobro(dobro x)
--4) Escreva uma função que, dadas as medidas dos catetos de um triângulo retângulo, retorne o valor de sua hipotenusa.
hipotenusa a b = sqrt(a^2+b^2)
--5) Escreva uma função para calcular a distância entre dois pontos A e B num plano cartesiano. 
distancia x1 y1 x2 y2 = sqrt((x2 - x1)^2+(y2 - y1)^2)