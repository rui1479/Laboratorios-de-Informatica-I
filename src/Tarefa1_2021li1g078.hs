{- |
Module      : Tarefa1_2021li1g078
Description : Validação de um potencial mapa
Copyright   :  Rui Pedro Fernandes Ribeiro <a100816@alunos.uminho.pt>;
            : Pedro Andrade Carneiro <a100652@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g078 where

import LI12122
import GHC.Exts (Int)
{- | A função __ValidaPotencialMapa__ valida se uma dada lista de __[(Peca,Coordenada)]__ é válida para ser construído um mapa, para tal, esta assenta em 5 condições.


== Exemplos de utilização:

>>> validaPotencialMapa [(Bloco, (0,0)), (Bloco, (0,1)), (Bloco, (1,0)), (Bloco , (1,1))]
False
>>> validaPotencialMapa [(Vazio, (0,0)), (Bloco, (0,1)), (Porta, (1,0)), (Bloco , (1,1))]
True


== Propriedades:

 1- Não pode haver mais do que uma declaração de peça para a mesma posição.

 2- Tem que estar declarado pelo menos uma porta.

 3- Todas as caixas devem estar posicionadas em cima de outra caixa ou bloco, i.e. não podem haver caixas a “flutuar”.

 4- Devem existir espaços vazios (no mínimo um), i.e. o mapa não pode estar totalmente preenchido por caixas, blocos e porta.

 5- A base do mapa deve ser composta por blocos, i.e. deve existir um chão ao longo do mapa.


 -}
 --Função que valida um potencial mapa em função dos 5 parâmetros pedidos
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa []= False 
validaPotencialMapa l = testacoord (separa l) && validaporta l  && validacaixa (caixabloco (ordena l)) && validavazio l && validachao l

--1
{- | A função __testacoord__  verifica a __primeira__ condição da __validaPotencialMapa__ visto que, verifica se existem declarações com coordenadas repetidas.


== Exemplos de utilização:

>>> testacoord [(Vazio, (0,0)), (Bloco, (0,0)), (Porta, (1,0)), (Bloco , (1,1))]
False
>>> testacoord [(Vazio, (0,0)), (Bloco, (0,1)), (Porta, (1,0)), (Bloco , (1,1))]
True

 -}
 
--Testa Repetições de Coordenadas
testacoord :: [Coordenadas] -> Bool
testacoord [] = True 
testacoord (h:t) = notElem h t && testacoord t

{- | Função que separa a lista dada em listas de coordenadas das respetivas declarações.

  -}  

--Separa lista em lista de Coordenadas
separa :: [(Peca,Coordenadas )] -> [Coordenadas]
separa [] = []
separa ((peca,coord):t) = coord : separa t 

--2

{- | A função __validaporta__ verifica a __segunda__ condição da __validaPotencialMapa__ visto que, verifica se existe exatamente uma porta.


== Exemplos de utilização:

>>> validaporta [(Vazio, (0,0)), (Bloco, (0,1)), (Bloco, (1,0)), (Bloco , (1,1))]
False
>>> validaporta [(Vazio, (0,0)), (Bloco, (0,1)), (Vazio, (1,0)), (Bloco , (1,1))]
True

 -}

--Testa a existência de uma só porta
validaporta :: [(Peca, Coordenadas)] -> Bool
validaporta [] = False
validaporta l = contaporta l == 1

{- | Função que conta o número de portas presentes na lista dada.
 
 -} 

--Conta número de portas 
contaporta :: [(Peca, Coordenadas)] -> Int
contaporta [] = 0
contaporta ((peca ,_):t) | peca == Bloco = 0 + contaporta t
                         | peca == Porta = 1 + contaporta t
                         | peca == Caixa  = 0 + contaporta t
                         | peca == Vazio  = 0 + contaporta t

--3 
{- | A função __validacaixa__  verifica a __terceira__ condição da __validaPotencialMapa__ visto que, verifica se todas as caixas estão em cima de um bloco ou de uma caixa, isto é, não haver caixas a flutuar.


== Exemplos de utilização:

>>> validacaixa [(Caixa, (0,0)), (Vazio, (0,1)), (Porta, (1,0)), (Bloco , (1,1))]
False
>>> validacaixa [(Caixa, (0,0)), (Bloco, (0,1)), (Porta, (1,0)), (Bloco , (1,1))]
True

 -}

--Verifica se existe caixas "flutuantes" 
validacaixa :: [(Peca, Coordenadas)] -> Bool
validacaixa [] = True
validacaixa ((peca, (x,y)):t) 
    | peca == Caixa = if elem (Caixa, (x,y+1)) t || elem (Bloco, (x,y+1)) t then validacaixa t else False
    | otherwise = validacaixa t

{- | Função que cria uma lista das declarações onde a peça seja Caixa ou Bloco a partir da lista dada.
 
 -} 

--Cria lista de caixas e blocos 
caixabloco :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
caixabloco [] = []
caixabloco ((peca, (x,y)):t) | peca == Caixa || peca == Bloco = (peca,(x,y)) : caixabloco t
                             | otherwise = caixabloco t
                        
{- | Função que ordena a lista dada em função das suas coordenadas crescentes, através do mecanismo isertion ISort.
 
 -} 

--Ordena listas através do mecanismo insertion iSort
ordena :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordena [] = []
ordena ((p1, (x,y)):t) = insere (p1, (x,y)) (ordena t)

{- | Auxiliar da ordena.
 
 -} 

insere :: (Peca, Coordenadas) -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
insere x [] = [x]
insere (p1 , (x,y)) ((p2, (x2,y2)):t) | (x,y) < (x2,y2) = (p1, (x,y)) : ((p2, (x2,y2)):t)
                                      | otherwise = (p2, (x2,y2)) : insere (p1 , (x,y)) t
--4

{- | A função __validavazio__ verifica a __quarta__ condição da __validaPotencialMapa__ visto que, verifica se existe pelo menos um vazio.


== Exemplos de utilização:

>>> validavazio [(Bloco, (0,0)), (Bloco, (0,1)), (Porta, (1,0)), (Bloco , (1,1))]
False
>>> validavazio [(Vazio, (0,0)), (Bloco, (0,1)), (Porta, (1,0)), (Bloco , (1,1))]
True

 -}

--Valida a existencia de pelo menos um vazio
validavazio ::[(Peca, Coordenadas)] -> Bool 
validavazio [] = True 
validavazio l |length l < espacosgrelha l = True     --caso vazios estejam omitidos
              |length l == espacosgrelha l = contavazio l /= 0 --caso vazios estejam declarados
              |otherwise = False

{- | Função que conta os espaços totais da grelha do potencial mapa através da coordenada máxima.

 -}

--Conta os espacos na grelha do jogo
espacosgrelha :: [(Peca, Coordenadas)] -> Int
espacosgrelha l = quantespacos(cordmaxima (separa l))
    where 
        quantespacos :: Coordenadas -> Int
        quantespacos (x,y) = (x+1) * (y+1)
            --Decobre a coordenada maxima
        cordmaxima :: [Coordenadas] -> Coordenadas
        cordmaxima [] = (0,0)
        cordmaxima ((x,y):t) = if (x,y) > cordmaxima t then (x,y) else cordmaxima t
    
{- | Função que conta o número de vazios da lista dada, usada quando os vazios não estão omitidos.

 -}

--Conta numero de vazios
contavazio :: [(Peca, Coordenadas)] -> Int
contavazio [] = 0
contavazio ((peca ,_):t) | peca == Bloco = 0 + contavazio t
                         | peca == Porta = 0 + contavazio t
                         | peca == Caixa  = 0 + contavazio t
                         | peca == Vazio  = 1 + contavazio t

--5
{- | A função __validachão__ verifica a __quinta__ condição da __validaPotencialMapa__ visto que, verifica se existe um chão composto por blocos e se este é contínuo.


== Exemplos de utilização:

>>> validavazio [(Vazio, (0,0)), (Vazio, (0,1)), (Porta, (1,0)), (Bloco , (1,1))]
False
>>> validavazio [(Vazio, (0,0)), (Bloco, (0,1)), (Porta, (1,0)), (Bloco , (1,1))]
True

 -}

--Valida se a base do mapa é composto por blocos e se estes sao interligados entre si (contínuos)
validachao:: [(Peca, Coordenadas)] -> Bool
validachao l = testabloco l && testacontinuidade (ultimapeca (separalistas l))
    where
        --Testa se em relação ao bloco mais a baixo, existe num raio de um bloco outro bloco, para haver continuidade
        testacontinuidade :: [(Peca, Coordenadas)] -> Bool 
        testacontinuidade [] = True
        testacontinuidade [p,(x,y)] = True
        testacontinuidade ((p, (x,y)):(p2,(x2,y2)):t) 
            | ((x+1),y) == (x2,y2) = testacontinuidade ((p2,(x2,y2)):t)      -- Verifica se é contínua num raio de 1 bloco para a
            | ((x+1),(y-1)) ==  (x2,y2) = testacontinuidade ((p2,(x2,y2)):t) -- frente, frente e cima, frente e baixo respetivamente
            | ((x+1),(y+1)) ==  (x2,y2) = testacontinuidade ((p2,(x2,y2)):t) 
            | otherwise =if y2< y --Descobrir se a continuidade é para cima ou para baixo
                         then if porcima (p, (x,y)) l then testacontinuidade ((p, (x,y-1)): (p2,(x2,y2)):t) else False  -- Testa a continuidade usando o bloco acima
                         else if porbaixo (p, (x,y)) l then testacontinuidade ((p, (x,y+1)): (p2,(x2,y2)):t) else False -- Testa a continuidade usando o bloco abaixo
                            where
                                -- Descobre se a peça acima é um bloco 
                                porcima :: (Peca, Coordenadas) -> [(Peca, Coordenadas)] -> Bool
                                porcima _ [] = False
                                porcima (p, (x,y)) ((p2, (x2,y2)):t) | p==p2 = if (x,y-1) == (x2,y2) then True else porcima (p, (x,y)) t
                                                                     |otherwise = porcima (p, (x,y)) t
                                    -- Descobre se a peça abaixo é um bloco 
                                porbaixo :: (Peca, Coordenadas) -> [(Peca, Coordenadas)] -> Bool
                                porbaixo _ [] = False
                                porbaixo (p, (x,y)) ((p2, (x2,y2)):t) | p==p2 = if (x,y+1) == (x2,y2) then True else porbaixo (p, (x,y)) t
                                                                      | otherwise = porbaixo (p, (x,y)) t

{- | Função que verifica se o chão é composto por blocos.

 -}

--Valida se a base do mapa é composto por blocos
testabloco :: [(Peca, Coordenadas)] -> Bool 
testabloco l = validaultimapeca (ultimapeca (separalistas l))

{- | Função que testa se o ultimo elemento todos os elementos da lista são Blocos.

 -}

--Testa se o y maximo é um bloco para todo o chão
validaultimapeca :: [(Peca, Coordenadas)] -> Bool 
validaultimapeca [] = True
validaultimapeca ((p,(_,_)):xs) | p == Bloco = validaultimapeca xs
                                | otherwise = False

{- | Função que cria uma lista dos ultimos elementos de cada lista numa lista de listas.

 -}

--Cria uma lista dos ultimos (Peca,Coordenada) da lista de listas.
ultimapeca :: [[(Peca, Coordenadas)]] -> [(Peca, Coordenadas)]
ultimapeca [] = []
ultimapeca (x:xs) = last x : ultimapeca xs

{- | Função que cria uma lista de listas conforma o mesmo valor de x de cada declaração.

 -}

--Cria uma lista de listas ordenada aonde cada lista tem o mesmo valor de x, e y crescente
separalistas:: [(Peca, Coordenadas)] -> [[(Peca, Coordenadas)]]
separalistas [] = []
separalistas l = crialistas (ordena l)

{- | Auxiliar da separalistas.

 -}
crialistas :: [(Peca, Coordenadas)] -> [[(Peca, Coordenadas)]]
crialistas [] = []
crialistas [x] = [[x]]
crialistas ((p, (x,y)):t)   | elemx (x,y) (head r) = ((p, (x,y)): head r) : tail r
                            | otherwise = [(p, (x,y))] :r
                where r = crialistas t

{- | Função que testa se uma dada coordenada pertence na lista dada.

 -}
 
--Função elem para Coordenadas
elemx :: Coordenadas -> [(Peca, Coordenadas)] -> Bool
elemx _ [] = False
elemx (x,y) ((p,(x2,y2)):t) = if x == x2 then True else elemx (x,y) t


                


                   



    
