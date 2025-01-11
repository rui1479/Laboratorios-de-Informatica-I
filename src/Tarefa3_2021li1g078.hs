{- |
Module      : Tarefa3_2021li1g078
Description : Representação textual do jogo
Copyright   :  Rui Pedro Fernandes Ribeiro <a100816@alunos.uminho.pt>;
            : Pedro Andrade Carneiro <a100652@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g078 where

import LI12122

instance Show Jogo where
 show = tudo

{- | A função __Tudo__ é a função que transforma os dados __Jogo__ em __Strings__ podendo assim ver/ler o jogo. 


== Exemplos de utilização:

>>> tudo (Jogo [[Vazio,Vazio],[Bloco,Bloco]] (Jogador (1,0) Oeste False))
" <\nXX"


== Propriedades:

__"/n"__ reprensenta a troca de linha.

__" "__ representam os Vazios.

__"C"__ representam as Caixas.

__"X"__ representam os Blocos.

__">"__ ou__"<"__ representam o jogador e a sua respetiva direção.

O jogador também pode ter uma caixa, e nesse caso a caixa aparece por cima deste.

O jogador so pode pegar numa caixa se esta tiver a frente do jogador.


 -}

--Função que transforma o jogo em String (Show)
tudo ::  Jogo -> String
tudo (Jogo (p:ps) (Jogador (x,y) direcao temCaixa)) | not temCaixa = mostrajogador (juntajogador (Jogador (x,y) direcao temCaixa) r)
                                                    | temCaixa = poecaixa (Jogo (p:ps) (Jogador (x,y) direcao False))
  where 
        n = metejogador (Jogo (p:ps) (Jogador (x,y) direcao temCaixa)) l
        l = showJogo (Jogo (p:ps) (Jogador (x,y) direcao temCaixa))
        r = eliminasegundo (splitfuncao n l)


---Caso temCaixa== False

{- | Função que mostra o Jogo em forma de string mas sem pôr o jogador no sua respetiva posição.
 
 -} 

--Mostra o mapa em forma de String sem o jogador
showJogo :: Jogo -> String
showJogo (Jogo mapa (Jogador coordenadas direcao temCaixa)) =init(unlines $ map concat $ map (map showpeca) mapa)

{- | Função indica o tamanho de cada linha do mapa.
 
 -} 

--Indica o tamanho de cada linha do mapa
tamanholinha :: Mapa -> Int
tamanholinha ((x:xs):t) = length (x:xs)

{- | Indica qual a posição do jogador na string através das suas coordenadas.
 
 -} 

--Calcula aonde é para colocar o jogador
metejogador :: Jogo -> String -> Int
metejogador (Jogo (p:ps) (Jogador (x,y) direcao temCaixa)) l = (n+1)*y + x 
  where n = length p --tamanholinha
        l = showJogo (Jogo (p:ps) (Jogador (x,y) direcao temCaixa))

{- | Função que separa a string em 2.
 
 -} 

--Dividir a função no sitio que quero por o jogador
splitfuncao ::  Int -> String -> ([Char],[Char])
splitfuncao  n (h:t) = splitAt n (h:t)   --saber aonde quero dividir a string

{- | Função que elimina a cabeça da segunda componento do "corte".
 
 -} 

--Elimina o primeiro elemento da segunda parta do "corte"
eliminasegundo :: ([Char],[Char]) -> ([Char],[Char])
eliminasegundo  ((x:xs),(y:ys)) = ((x:xs), ys) 

{- | Função que coloca o jogador na sua respetiva posição tendo em conta a sua direção.
 
 -} 

--Mete o jogador no sitio segundo a direção em que está virado
juntajogador :: Jogador -> ([Char],[Char]) -> ([Char],[Char])
juntajogador (Jogador (a,b) direcao temCaixa) (e,d) | direcao == Oeste = (e , "<" ++ d) --poe o jogador no sitio
                                                    | direcao == Este = (e , ">" ++ d)

{- | Função que transforma o par de listas numa string.
 
 -} 

--Junta ambas as listas para dar uma String so com o jogador ja colocado
mostrajogador :: ([Char],[Char]) -> String
mostrajogador (x,y) = x ++ y --junta as duas listas

{- | Função aonde cada __Peca__ tem uma respetiva representação.
 
 -} 

showpeca :: Peca -> String
showpeca p = case p of 
  Vazio -> " "
  Bloco -> "X"
  Caixa -> "C"
  Porta -> "P"

{- | Função aonde o jogador tem uma respetiva representação dependendo da sua direção.
 
 -} 

showjogador :: Jogador -> String
showjogador ( Jogador coordenadas direcao temcaixa) = case direcao of
    Este -> ">"
    Oeste -> "<"

--Caso Temcaixa== True

{- | Função utilizada caso __temcaixa == True__ aonde visa pôr a caixa na posição acima do jogador.
 
 -} 

--Função que põe a caixa em cima do jogador
poecaixa ::Jogo -> String
poecaixa (Jogo (p:ps) (Jogador (x,y) direcao temCaixa)) = mostrajogador (juntacaixa (eliminasegundo (separacaixa (Jogo (p:ps) (Jogador (x,y) direcao temCaixa)))))

{- | Função que junta as duas partes do corte mas com um __"C"__ no meio.
 
 -} 

--Junta as duas parte com "C" no meio
juntacaixa :: ([Char],[Char]) -> ([Char],[Char])
juntacaixa (e,d) = (e , "C" ++ d) 

{- | Função que separa a string na posição diretamente acima do jogador.
 
 -} 

--Separa a String na ordenada do jogador -1
separacaixa :: Jogo -> ([Char], [Char])
separacaixa (Jogo (p:ps) (Jogador (x,y) direcao temCaixa))  = splitfuncao n l
  where n = descobrejogador l - length p -1
        l = tiracaixa (Jogo (p:ps) (Jogador (x,y) direcao temCaixa))

{- | Função que tira a caixa da string e mete um vazio.
 
 -} 

--Tira a caixa da string e mete um vazio
tiracaixa :: Jogo -> String
tiracaixa (Jogo (p:ps) (Jogador (x,y) direcao temCaixa)) = mostrajogador (juntavazio (eliminasegundo (divide (Jogo (p:ps) (Jogador (x,y) direcao temCaixa)))))

{- | Função que separa a string na posição da caixa a ser tirada.
 
 -} 

--Dividir na posição da caixa
divide :: Jogo -> ([Char], [Char])
divide (Jogo (p:ps) (Jogador (x,y) direcao temCaixa)) = splitfuncao n l
  where n = descobrecaixa l
        l= tudo (Jogo (p:ps) (Jogador (x,y) direcao temCaixa)) 

{- | Função que coloca um vazio na cabeça da segunda componente do par.
 
 -} 

--Mete um vazio na cabeça da segunda parte
juntavazio :: ([Char],[Char]) -> ([Char],[Char])
juntavazio (e,d) = (e , " " ++ d) --poe o vazio no sitio

{- | Função que indica a posição da caixa na String.
 
 -} 

--Indica a posição da caixa na String
descobrecaixa :: String -> Int
descobrecaixa [] = 0
descobrecaixa (x:xs) = if x == 'C' || x == 'C' then 0 else 1+ descobrecaixa xs

{- | Função que indica a posição do jogador na String.

 -}

--Indica a posição do jogador na String
descobrejogador :: String -> Int
descobrejogador [] = 0
descobrejogador (x:xs) = if x == '<' || x == '>' then 0 else 1+ descobrejogador xs





