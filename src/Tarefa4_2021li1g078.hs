{- |
Module      : Tarefa4_2021li1g078
Description : Movimentação do personagem
Copyright   :  Rui Pedro Fernandes Ribeiro <a100816@alunos.uminho.pt>;
            : Pedro Andrade Carneiro <a100652@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g078 where

import LI12122
import Tarefa2_2021li1g078

correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos j [m] = moveJogador j m
correrMovimentos j (x:xs) = correrMovimentos (moveJogador j x) xs

-- | Coordenadas das Peças
coor :: Mapa -> Coordenadas -> [(Peca, Coordenadas)]
coor [] _ = []
coor ([]:ps) (x,y) = coor ps (0,y+1)
coor ((h:t):ps) (x,y) = (h,(x,y)) : coor (t:ps) (x+1,y) 

-- | Retira Peça da lista de Peças e Coordenadas
retirarpeça :: [(Peca, Coordenadas)] -> Coordenadas -> Peca
retirarpeça [] _ = Bloco
retirarpeça ((p,(x1,y1)):t) (x2,y2) = if (x1,y1) == (x2,y2) then p else retirarpeça t (x2,y2)

-- | Troca o Vazio por uma Caixa
deixarcaixa :: [(Peca, Coordenadas)] -> Coordenadas -> [(Peca, Coordenadas)]
deixarcaixa [] (x2,y2) = []
deixarcaixa ((p,(x1,y1)):t) (x2,y2) = if (x1,y1) == (x2,y2) then (Caixa,(x1,y1)) : t else (p,(x1,y1)) : deixarcaixa t (x2,y2)

-- | Troca a Caixa por Vazio
levantarcaixa :: [(Peca, Coordenadas)] -> Coordenadas -> [(Peca, Coordenadas)]
levantarcaixa [] (x2,y2) = []
levantarcaixa ((p,(x1,y1)):t) (x2,y2) = if (x1,y1) == (x2,y2) then (Vazio,(x1,y1)) : t else (p,(x1,y1)) : levantarcaixa t (x2,y2) 

-- | Coordenadas da queda da Peça
queda :: [(Peca, Coordenadas)] -> Coordenadas -> Direcao -> Coordenadas
queda p (x,y) b | b == Oeste = if retirarpeça p (x-1,y) == Vazio then queda p (x,y+1) b else (x-1,y-1) 
                | b == Este = if retirarpeça p (x+1,y) == Vazio then queda p (x,y+1) b else (x+1,y-1)


moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo p (Jogador (x,y) b m)) AndarEsquerda = if b == Oeste then if m == False then if pecadaesquerda == Vazio || pecadaesquerda == Porta then if pecadebaixo == Vazio || pecadebaixo == Porta  then moveJogador (Jogo p (Jogador (x,y+1) b m)) AndarEsquerda else Jogo p ( Jogador (x-1,y) b m) else Jogo p (Jogador (x,y) b m) else if pecadaesquerda == Vazio && pecadecima == Vazio then if pecadebaixo == Vazio then moveJogador (Jogo p (Jogador (x,y+1) b m)) AndarEsquerda else Jogo p (Jogador (x-1,y) b m) else Jogo p (Jogador (x,y) b m) else moveJogador (Jogo p(Jogador (x,y) Oeste m)) AndarEsquerda
        where pecadaesquerda = retirarpeça (coor p (0,0)) (x-1,y)
              pecadebaixo = retirarpeça (coor p (0,0)) (x-1,y+1)
              pecadecima = retirarpeça (coor p (0,0)) (x-1,y-1)
moveJogador (Jogo p (Jogador (x,y) b m)) AndarDireita = if b == Este then if m == False then if pecadadireita == Vazio || pecadadireita == Porta then if pecadebaixodireita == Vazio then moveJogador (Jogo p (Jogador (x,y+1) b m)) AndarDireita else Jogo p (Jogador (x+1,y) b m) else Jogo p (Jogador (x,y) b m) else if pecadadireita == Vazio && pecacimadireita == Vazio then if pecadebaixodireita == Vazio then moveJogador (Jogo p (Jogador (x,y+1) b m)) AndarDireita else Jogo p (Jogador (x+1,y) b m) else Jogo p (Jogador (x,y) b m) else moveJogador (Jogo p(Jogador (x,y) Este m)) AndarDireita 
        where pecadadireita = retirarpeça (coor p (0,0)) (x+1,y)
              pecadebaixoesquerda = retirarpeça (coor p (0,0)) (x-1,y+1)
              pecadebaixodireita = retirarpeça (coor p (0,0)) (x+1,y+1)
              pecadecima = retirarpeça (coor p (0,0)) (x-1,y-1)
              pecacimadireita = retirarpeça (coor p (0,0)) (x+1,y-1)
moveJogador (Jogo p (Jogador (x,y) b m)) Trepar
    | b == Este && (pecadadireita == Bloco || pecadadireita == Caixa) = if m == False then if pecacimadireita == Vazio then Jogo p (Jogador (x+1,y-1) b m) else Jogo p (Jogador (x,y) b m) else if pecacimadireita == Vazio && pecacimadireita2 == Vazio then Jogo p (Jogador (x+1,y-1) b m) else Jogo p (Jogador (x,y) b m)
    | b == Oeste && (pecadaesquerda == Bloco || pecadaesquerda == Caixa) = if m == False then if pecacimaesquerda == Vazio || pecacimaesquerda == Porta then Jogo p (Jogador (x-1,y-1) b m) else Jogo p (Jogador (x,y) b m) else if pecacimaesquerda == Vazio && pecacimaesquerda2 == Vazio then Jogo p (Jogador (x-1,y-1) b m) else Jogo p (Jogador (x,y) b m)
    | otherwise = Jogo p (Jogador (x,y) b m)
        where pecadaesquerda = retirarpeça (coor p (0,0)) (x-1,y)
              pecadadireita = retirarpeça (coor p (0,0)) (x+1,y)
              pecacimaesquerda = retirarpeça (coor p (0,0)) (x-1,y-1)
              pecacimadireita = retirarpeça (coor p (0,0)) (x+1,y-1)
              pecacimaesquerda2 = retirarpeça (coor p (0,0)) (x-1,y-2)
              pecacimadireita2 = retirarpeça (coor p (0,0)) (x+1,y-2)
moveJogador (Jogo p (Jogador (x,y) b m)) InterageCaixa
    | b == Oeste = if m == False then if pecadaesquerda == Caixa && pecacima == Vazio && pecacimaesquerda == Vazio then Jogo retiraesquerda (Jogador (x,y) b True) else Jogo p (Jogador (x,y) b m) else if (pecadaesquerda == Vazio || pecacimaesquerda == Vazio) && pecadaesquerda /= Porta then Jogo caixamapa (Jogador (x,y) b False) else Jogo p (Jogador (x,y) b m)
    | b == Este = if m == False then if pecadadireita == Caixa && pecacima == Vazio && pecacimadireita == Vazio && m == False then Jogo retiradireita (Jogador (x,y) b True) else Jogo p (Jogador (x,y) b m) else if (pecadadireita == Vazio || pecacimadireita == Vazio) && pecadadireita /= Porta then Jogo caixamapa (Jogador (x,y) b False) else Jogo p (Jogador (x,y) b m)
         where pecadaesquerda = retirarpeça (coor p (0,0)) (x-1,y)
               pecadadireita = retirarpeça (coor p (0,0)) (x+1,y)
               pecacimaesquerda = retirarpeça (coor p (0,0)) (x-1,y-1)
               pecacimadireita = retirarpeça (coor p (0,0)) (x+1,y-1)
               caixamapa = constroiMapa (deixarcaixa (coor p (0,0)) local)
               retiraesquerda = constroiMapa (levantarcaixa (coor p (0,0)) (x-1,y))
               retiradireita = constroiMapa (levantarcaixa (coor p (0,0)) (x+1,y))
               pecacima = retirarpeça (coor p (0,0)) (x,y-1)
               local = queda (coor p (0,0)) (x,y) b