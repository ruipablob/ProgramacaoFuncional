-- 1) Implemente as funções recursivas vistas nas vídeo-aulas 9 e 10: conta_ch,
-- conta, maior, primeiros, pertence, uniaoR
conta_ch :: [Char] -> Int
conta_ch [] = 0
conta_ch (x:resto) = 1 + conta_ch resto

conta :: [t] -> Int
conta [] = 0
conta (x:r) = 1 + conta r

maior :: [Int] -> Int
maior [x] = x
maior (x:y:resto)
  |x>y = maior (x:resto)
  |otherwise = maior (y:resto)

primeiros :: Int -> [t] -> [t]
primeiros 0 _ = []
primeiros _ [] = []
primeiros n (x:xs) = x : primeiros (n-1) xs

pertence :: Eq t => t -> [t] -> Bool
pertence a [] = False
pertence a (x:xs) = if (a == x) then True else pertence a xs

uniaoR :: Eq t => [t] ->  [t] -> [t]
uniaoR [] l = l
uniaoR (x:xs) l = if pertence x l then uniaoR xs l else x: uniaoR xs l

-- 2) Escreva a função recursiva npares que recebe uma lista de inteiros e retorna a
-- quantidade de números pares pertencentes à lista.
n_pares :: [Int] -> Int
n_pares [] = 0
n_pares (x:xs) = if even x == True then 1 + n_pares xs else n_pares xs

-- 3) Escreva a função recursiva produtorio que recebe uma lista de números e
-- retorna o produto de todos os seus elementos
produtorio :: [Int] -> Int
produtorio [] = 1
produtorio (x:xs) = x * produtorio xs

-- 4) Escreva a função recursiva comprime a seguir que recebe uma lista de listas e
-- retorna uma lista contendo todos os elementos das sublistas.
-- > comprime [[1,2],[3,4,5],[],[6]]
-- [1,2,3,4,5,6]
comprime :: [[Int]] -> [Int]
comprime [[]] = []
comprime [[x]] = [x]
comprime (x:xs) = x ++ comprime xs

-- 5) Escreva a função recursiva tamanho a seguir que recebe uma lista polimórfica
-- (de qualquer tipo) e retorna a quantidade de elementos que ela possui.
-- > tamanho [1,3,5,7,9]
-- 5
tamanho :: Eq t => [t] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

-- 6) Escreva a função recursiva uniaoRec2 que faz a união de duas listas de modo
-- que mantenha todos os elementos da 1a lista na mesma ordem e no final
-- acrescenta os elementos da 2a lista que não estejam presentes na primeira.
-- > uniaoRec2 [1,2,3,4,5,6,7] [2,9,7,10,4]
-- [1,2,3,4,5,6,7,9,10]
compara :: Int -> [Int] -> [Int]
compara x [] = []
compara x (h:t) 
 |x == h = compara x t 
 |otherwise = h:compara x t 

uniaoRec2 :: [Int] -> [Int] -> [Int]
uniaoRec2 [x] l = x:compara x l
uniaoRec2 (h:t) l = h:uniaoRec2 t (compara h l)