{- |
Module      : Tarefa6_2021li1g078
Description : Resolução de um puzzle
Copyright   :  Rui Pedro Fernandes Ribeiro <a100816@alunos.uminho.pt>;
            : Pedro Andrade Carneiro <a100652@alunos.uminho.pt>;

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}

module Tarefa6_2021li1g078 where

import LI12122
import Tarefa4_2021li1g078

{- | A função __resolveJogo__ tenta resolver um dado jogo num número máximo de tentativas.


== Exemplos de utilização:

@
Seja: 
   m1 :: Mapa
   m1 = [
          [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
          [Porta, Vazio, Bloco, Vazio, Vazio, Vazio],
          [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
@

>>> resolveJogo 3 (Jogo m1 (Jogador (3,1) Oeste False))
Just [Trepar, AndarEsquerda, AndarEsquerda]


>>> resolveJogo 2 (Jogo m1 (Jogador (3,1) Oeste False))
Nothing


>>> resolveJogo 0 (Jogo m1 (Jogador (3,1) Oeste False))
Nothing


>>> resolveJogo 0 (Jogo m1 (Jogador (0,1) Oeste False))
Just []

== Propriedades:

 1- Se o número máximo imposto para resolver o jogo for igual a 0, caso o jogador já se encontre na porta retorna __Just []__, caso não se encontre retorna __Nothing__.

 2- Se o número máximo imposto para resolver o jogo for menor do que o número mínimo de movimentos para concluir o nível, a função retorna __Nothing__.

 3- Se o número máximo imposto para resolver o jogo for igual ao número mínimo de movimentos para concluir o nível, a função retorna a lista com o menor número de movimentos possíveis.
    
 4- Se o número máximo imposto para resolver o jogo for maior do que o número mínimo de movimentos para concluir o nível, a função retorna uma das listas que conclui o nível, não sendo esta a com menor número de movimentos possíveis.


 -}
 
resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo n jogo@(Jogo mapa (Jogador (x,y) dir temcaixa)) | n < 0 = Nothing
                                                            | n == 0 = if posicaoatual jogo  == Porta 
                                                                       then Just []
                                                                       else Nothing
                                                            | otherwise = if null todas 
                                                                          then Nothing
                                                                          else Just (head todas)
    where
        todas = tentaTodas n [jogo] []

{- | A função __posicaoatual__  indica qual a peça localizada nas coordenadas do jogador.


== Exemplos de utilização:

>>> posicaoatual (Jogo m1 (Jogador (3,1) Oeste False))
Vazio

>>> posicaoatual (Jogo m1 (Jogador (0,1) Oeste False))
Porta

 -}

posicaoatual :: Jogo -> Peca
posicaoatual (Jogo mapa (Jogador (x,y) dir temcaixa)) = (mapa !! y ) !! x

{- | A função __tentaTodas__ cria uma lista de listas de todas as combinações de movimentos possíveis, com um certo tamanho imposto, usando as funções __tryD__, __tryE__, __tryInt__, __tryTre__.

 -}

tentaTodas :: Int -> [Jogo] -> [Movimento] -> [[Movimento]]
tentaTodas n lista@(jogo@(Jogo mapa (Jogador (x,y) dir temcaixa)):t) mov = if posicaoatual jogo == Porta 
                                                                           then [mov]
                                                                           else if n == 0 
                                                                                then []
                                                                                else  tryD n lista mov ++ 
                                                                                       tryE n lista mov ++
                                                                                       tryInt n lista mov ++ 
                                                                                       tryTre n lista mov
                                                                                    
{- | A função __tryD__ cria uma lista de listas de todas os possíveis movimentos __AndarDireita__, com um certo tamanho imposto.

 -}

tryD :: Int -> [Jogo] -> [Movimento] -> [[Movimento]]
tryD  n lista@(jogo@(Jogo mapa (Jogador (x,y) dir temcaixa)):t) mov | dir `elem` lista = []
                                                                    | otherwise = tentaTodas (n-1) (dir : lista) (mov ++ [AndarDireita])
    where
        dir = moveJogador jogo AndarDireita

{- | A função __tryE__ cria uma lista de listas de todas os possíveis movimentos __AndarEsquerda__, com um certo tamanho imposto.

 -}

tryE :: Int -> [Jogo] -> [Movimento] -> [[Movimento]]
tryE n lista@(jogo@(Jogo mapa (Jogador (x,y) dir temcaixa)):t) mov | esq `elem` lista = []
                                                                   | otherwise = tentaTodas (n-1) (esq : lista) (mov ++ [AndarEsquerda])
    where
        esq = moveJogador jogo AndarEsquerda

{- | A função __tryInt__ cria uma lista de listas de todas os possíveis movimentos __InterageCaixa__, com um certo tamanho imposto.

 -}

tryInt :: Int -> [Jogo] -> [Movimento] -> [[Movimento]]
tryInt n lista@(jogo@(Jogo mapa (Jogador (x,y) dir temcaixa)):t) mov | int `elem` lista = []
                                                                     | otherwise = tentaTodas (n-1) (int : lista) (mov ++ [InterageCaixa])
    where
        int = moveJogador jogo InterageCaixa

{- | A função __tryTre__ cria uma lista de listas de todas os possíveis movimentos __Trepar__, com um certo tamanho imposto.

 -}

tryTre :: Int -> [Jogo] -> [Movimento] -> [[Movimento]]
tryTre n lista@(jogo@(Jogo mapa (Jogador (x,y) dir temcaixa)):t) mov | tre `elem` lista = []
                                                                     | otherwise = tentaTodas (n-1) (tre : lista) (mov ++ [Trepar])
    where
        tre = moveJogador jogo Trepar