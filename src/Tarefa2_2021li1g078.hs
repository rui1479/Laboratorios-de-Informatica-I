{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- |
Module      : Tarefa2_2021li1g078
Description : Construção/Desconstrução do mapa
Copyright   :  Rui Pedro Fernandes Ribeiro <a100816@alunos.uminho.pt>;
            : Pedro Andrade Carneiro <a100652@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g078 where

import LI12122

-- | Constroi o Mapa desejado
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa l = foldr colocapeca (mapadevazios x y) l
    where
        x = maximoX l + 1
        y = maximoY l + 1

-- | Funções que calculam o maior Y da matriz
maximoY :: [(Peca,Coordenadas)] -> Int
maximoY [] = 0
maximoY l = maximum' (maxY l)

-- | Apresenta os Y da lista
maxY :: [(Peca,Coordenadas)] -> [Int]
maxY [] = []
maxY ((peca,(x,y)):tail) = y : maxY tail

-- | Funções que calculam o maior X da matriz
maximoX :: [(Peca,Coordenadas)] -> Int
maximoX [] = 0
maximoX l = maximum' (maxX l)

-- | Apresenta os X da lista
maxX :: [(Peca,Coordenadas)] -> [Int]
maxX [] = []
maxX ((peca,(x,y)):tail) = x : maxX tail

-- | Retoma o valor maximo
maximum' :: Ord a => [a] -> a
maximum' [x] = x
maximum' (x:x1:xs) = maximum' ((if x>=x1 then x else x1):xs)

-- | Cria matriz com tudo Vazio
mapadevazios :: Int -> Int -> Mapa
mapadevazios x y = replicate y  (replicate x Vazio)

-- | Coloca peca na matriz
colocapeca :: (Peca, Coordenadas) -> Mapa -> Mapa
colocapeca (peca, (x,y)) (h:t) = if y > 0 then h : colocapeca (peca, (x, y-1)) t else (colocalinha (peca, (x, y)) h) : t

-- | Coloca peca numa linha da matriz
colocalinha :: (Peca, Coordenadas) -> [Peca] -> [Peca]
colocalinha (pecanova, _) [] = [pecanova]
colocalinha (pecanova, (x, y)) (p:ps) = if x == 0 then pecanova : ps else p : colocalinha (pecanova, (x-1, y)) ps

-- | Deconstroi por linha
dlinha :: Int -> (Int, [Peca]) -> [(Peca, Coordenadas)]
dlinha n (x, []) = []
dlinha n (x, (Vazio:t)) = dlinha (n+1) (x, t)
dlinha n (x, (h:t)) = (h, (n,x)) : dlinha (n+1) (x, t)

-- | Desconstroi o Mapa feito para a lista de listas dada
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa m = concatMap (dlinha 0) ( zip [0..] m)