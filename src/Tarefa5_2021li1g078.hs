{- |
Module      : Tarefa5_2021li1g078
Description : Aplicação Gráfica
Copyright   :  Rui Pedro Fernandes Ribeiro <a100816@alunos.uminho.pt>;
            : Pedro Andrade Carneiro <a100652@alunos.uminho.pt>;

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import LI12122
import Tarefa4_2021li1g078
import Graphics.Gloss.Data.Bitmap

{- | O tipo do  __World__  é composto pelo __Menu__ e pela lista de jogos disponíveis.

 -}
 
type World = (Menu, [Jogo])

{- | O tipo do __WorldGloss__  é composto pelo __World__ e pelas imagens utilizadas. 

 -}

type WorldGloss = (World,(Picture,Picture,Picture,Picture,Picture,Float))

{- | Criação de um novo tipo __Opcoes__ que será utilizado para navegar pelo menu inicial.

 -}

data Opcoes = Jogar | Sair deriving Eq

{- | Criação de um novo tipo __Niveis__ para navegar entre os 3 níveis disponíveis.

 -}

data Niveis = Nivel1 | Nivel2 | Nivel3 deriving Eq

{- | Criação de um novo tipo __Ganhou__ para navegar entre as opções após completar um nível.

 -}

data Ganhou = Continuar | Voltar | Fim deriving Eq

{- | Criação de um novo tipo __Menu__ que serve para indicar qual modo de jogo utilizar.

 -}

data Menu = Controlador Opcoes | ModoJogo Jogo | VenceuJogo Ganhou| Seletor Niveis

{- | Jogo correspondente ao __Nível1__

 -}

m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (5, 3) Oeste False)

{- | Mapa do jogo __m1e1__

 -}

m1r :: Mapa
m1r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Caixa, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

{- | Jogo correspondente ao __Nível2__

 -}

m1e2 :: Jogo
m1e2 = Jogo m2r (Jogador (16,4) Oeste False)

{- | Mapa do jogo __m1e2__

 -}

m2r :: Mapa
m2r = 
  [ [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Porta, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

{- | Jogo correspondente ao __Nível3__

 -}

m1e3 :: Jogo
m1e3 = Jogo m3r (Jogador (18,6) Oeste False)

{- | Mapa do jogo __m1e3__

 -}

m3r :: Mapa
m3r = [ [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio],
        [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio],
        [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio],
        [Bloco, Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio],
        [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Caixa, Vazio, Caixa, Caixa, Vazio, Vazio, Vazio, Bloco],
        [Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Caixa, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
      ]

{- | Dimensões da janela criada pelo programa e as suas respetivas características.

 -}

window :: Display
window = InWindow 
            "BlockDude" -- título da janela
            (1500,780) -- dimensão da janela
            (0,0) -- posição no ecrã

{- | Frame Rate do programa.
 -}

fr :: Int
fr = 30

{- | Estado inicial do progama ao ser executado, sem imagens.

 -}

estadoinicial :: World
estadoinicial = (Controlador Jogar, [m1e1,m1e2,m1e3])

{- | Estado inicial do programa ao ser executado, com imagens.

 -}

estadoinicialgloss :: Picture -> Picture -> Picture ->Picture -> Picture ->  WorldGloss
estadoinicialgloss persE persO caixa bloco porta = (estadoinicial, (persE,persO,caixa, bloco, porta, 0.0))

{- | Função que apresenta o jogo na tela, dependendo em que menu está.

 -}

desenha :: WorldGloss -> Picture
desenha ((VenceuJogo Fim, jogo),e)  = Pictures [Translate (-700) (200) $ Color red $ Scale (0.7) (0.7) (Text "Nivel completado com sucesso!"), Translate (-300) (-70) $ Color blue $ Scale (0.5) (0.5) (Text "Voltar para o menu"), Translate (300) (-300) $ Color black $ Scale (0.2) (0.2) (Text "Clique enter para voltar") ]
desenha ((VenceuJogo Continuar, jogo),e)  = Pictures [Translate (-700) (200) $ Color red $ Scale (0.7) (0.7) (Text "Nivel completado com sucesso!") , Translate (-200) (-70) $ Color blue $ Scale (0.5) (0.5) (Text "Proximo Nivel"), Translate (-275) (-150) $ Scale (0.5) (0.5) (Text "Voltar para o menu") ]
desenha ((VenceuJogo Voltar, jogo),e)  = Pictures [Translate (-700) (200) $ Color red $ Scale (0.7) (0.7) (Text "Nivel completado com sucesso!") , Translate (-200) (-70) $ Scale (0.5) (0.5) (Text "Proximo Nivel"), Translate (-275) (-150) $ Color blue $ Scale (0.5) (0.5) (Text "Voltar para o menu") ]
desenha ((Controlador Jogar, jogo),e) = Pictures [Translate (-100) (10) $ Color blue $ desenhaopcoes Jogar, Translate (-30) (-70) $ desenhaopcoes Sair]
desenha ((Controlador Sair, jogo),e) = Pictures [Translate (-100) (10) $ desenhaopcoes Jogar, Color blue $ Translate (-30) (-70) $ desenhaopcoes Sair]
desenha ((Seletor Nivel1, jogo),e) = Pictures [Translate (-100) (10) $ Color blue $ desenhanivel Nivel1, Translate (-30) (-70) $ desenhanivel Nivel2, Translate (40) (-150) $ desenhanivel Nivel3]
desenha ((Seletor Nivel2, jogo),e) = Pictures [Translate (-100) (10) $ desenhanivel Nivel1, Color blue $ Translate (-30) (-70) $ desenhanivel Nivel2, Translate (40) (-150) $ desenhanivel Nivel3]
desenha ((Seletor Nivel3, jogo),e) = Pictures [Translate (-100) (10) $ desenhanivel Nivel1, Translate (-30) (-70) $ desenhanivel Nivel2, Color blue $ Translate (40) (-150) $ desenhanivel Nivel3]
desenha e@((ModoJogo (Jogo mapa jogador), jogo), (persE,persO, caixa, bloco, porta,b)) = Pictures $ desenhajogador e  : desenhatutorial e : desenhamapa mapa (0,0)
  where
    desenhamapa :: Mapa -> (Float,Float) -> [Picture]
    desenhamapa ((Bloco:xs):t) (x,y) = (Translate (-400 + x) (240-y) bloco) :(desenhamapa (xs:t) (x+c,y))
    desenhamapa ((Caixa:xs):t) (x,y) = (Translate (-400 + x) (240-y) caixa ):(desenhamapa (xs:t) (x+c,y))
    desenhamapa ((Porta:xs):t) (x,y) = (Translate (-400 + x) (240-y) porta ):(desenhamapa (xs:t) (x+c,y))
    desenhamapa ((Vazio:xs):t) (x,y) = Blank : desenhamapa (xs:t) (x+c,y) 
    desenhamapa ([]:t) (x, y) = desenhamapa t (0,y+c)
    desenhamapa [] _ = [Blank]

    desenhatutorial :: WorldGloss -> Picture
    desenhatutorial _ = Pictures [Translate (300) (-225) $ Scale (0.15) (0.15) (Text "Seta Esquerda <- Andar para a esquerda "),
                                  Translate (300) (-250) $ Scale (0.15) (0.15) (Text "Seta Direita <- Andar para a direita "),
                                  Translate (300) (-275) $ Scale (0.15) (0.15) (Text "Seta Cima <- Trepar "),
                                  Translate (300) (-300) $ Scale (0.15) (0.15) (Text "Seta Baixo <- Pegar/Largar a caixa "),
                                  Translate (300) (-325) $ Scale (0.15) (0.15) (Text "R <- Reiniciar o nivel")]

{- | Constante que representa a dimensão de cada peça.

 -}

c :: Float
c=48

{- | Função auxiliar que se encarrega de apresentar o jogador na tela.

 -}

desenhajogador :: WorldGloss -> Picture
desenhajogador ((ModoJogo (Jogo mapa (Jogador (x,y) dir temcaixa)), jogo), (persE,persO, caixa, bloco, porta,b))| temcaixa== False =  if dir == Oeste
                                                                then Translate (-400 +(i*c)) (240 - (j*c)) persO
                                                                else Translate (-400 +(i*c)) (240 - (j*c)) persE 
                                           | temcaixa == True = if dir == Oeste
                                                                then Pictures $ (Translate (-400 +(i*c)) (240 - (j*c)) persO) : [Translate (-400 +(i*c)) (240 - ((j-1)*c)) caixa]
                                                                else Pictures $ (Translate (-400 +(i*c)) (240 - (j*c)) persE) : [Translate (-400 +(i*c)) (240 - ((j-1)*c)) caixa]
                                                                
    where i = fromIntegral x
          j = fromIntegral y

{- | Função auxiliar que se encarrega de apresentar o menu de níveis disponiveis.

 -}

desenhanivel niveis | niveis == Nivel1 = Scale (0.5) (0.5) (Text "Nivel1")
                    | niveis == Nivel2 = Scale (0.5) (0.5) (Text "Nivel2")
                    | niveis == Nivel3 = Scale (0.5) (0.5) (Text "Nivel3")

{- | Função auxiliar que se encarrega de apresentar o menu inicial.

 -}

desenhaopcoes option | option == Jogar = Scale (0.5) (0.5) (Text "Jogar")
                     | option == Sair = Scale (0.5) (0.5) (Text "Sair")

{- | Função que determina as coordenadas da porta dado um __Mapa__

 -}

determinaporta :: Mapa -> Coordenadas -> Coordenadas 
determinaporta ((x:xs):t) (i,j) | elem Porta (x:xs) = if x==Porta then (i,j) else determinaporta (xs:t) (i+1,j)
                                | otherwise = determinaporta t (i,j+1)

{- | Função que se encarrega de alertar quando o jogador passou o nível. 

 -}

ganha :: Jogo -> Bool
ganha (Jogo mapa (Jogador (x,y) dir temcaixa)) = determinaporta mapa (0,0) == (x,y) 

{- | Função que reage um dado __WolrdGloss__ a um dado __Event__, isto é, reage às teclas a serem pressionadas.

 -}

evento :: Event-> WorldGloss-> WorldGloss
evento (EventKey (SpecialKey KeyEnter) Down _ _) ((Controlador Jogar, jogo),e) = ((Seletor Nivel1,jogo),e)
evento (EventKey (SpecialKey KeyEnter) Down _ _) ((Seletor Nivel1,jogo),e) = ((ModoJogo m1e1,[m1e1,m1e2,m1e3]),e)
evento (EventKey (SpecialKey KeyEnter) Down _ _) ((Seletor Nivel2,jogo),e) = ((ModoJogo m1e2,[m1e2,m1e3]),e)
evento (EventKey (SpecialKey KeyEnter) Down _ _) ((Seletor Nivel3,jogo),e) = ((ModoJogo m1e3,[m1e3]),e)
evento (EventKey (SpecialKey KeyDown) Down _ _) ((Seletor Nivel1,jogo),e) = ((Seletor Nivel2,jogo),e)
evento (EventKey (SpecialKey KeyUp) Down _ _) ((Seletor Nivel1,jogo),e) = ((Seletor Nivel3,jogo),e)
evento (EventKey (SpecialKey KeyDown) Down _ _) ((Seletor Nivel2,jogo),e) = ((Seletor Nivel3,jogo),e)
evento (EventKey (SpecialKey KeyUp) Down _ _) ((Seletor Nivel2,jogo),e) = ((Seletor Nivel1,jogo),e)
evento (EventKey (SpecialKey KeyUp) Down _ _) ((Seletor Nivel3,jogo),e) = ((Seletor Nivel2,jogo),e)
evento (EventKey (SpecialKey KeyDown) Down _ _) ((Seletor Nivel3,jogo),e) = ((Seletor Nivel1,jogo),e)
evento (EventKey (SpecialKey KeyEnter) Down a b) ((Controlador Sair, jogo),e) =undefined
evento (EventKey (SpecialKey KeyDown) Down _ _) ((Controlador Jogar, jogo),e) = ((Controlador Sair, jogo),e)
evento (EventKey (SpecialKey KeyUp) Down _ _) ((Controlador Jogar, jogo),e) = ((Controlador Sair, jogo),e)
evento (EventKey (SpecialKey KeyUp) Down _ _) ((Controlador Sair, jogo),e) = ((Controlador Jogar, jogo),e)
evento (EventKey (SpecialKey KeyDown) Down _ _) ((Controlador Sair, jogo),e) = ((Controlador Jogar, jogo),e)
evento (EventKey (SpecialKey KeyLeft) Down _ _) ((ModoJogo p, jogo),e) = ((ModoJogo (moveJogador p AndarEsquerda),jogo),e)
evento (EventKey (SpecialKey KeyRight) Down _ _) ((ModoJogo p, jogo),e) = ((ModoJogo (moveJogador p AndarDireita),jogo),e)
evento (EventKey (SpecialKey KeyUp) Down _ _) ((ModoJogo p, jogo),e) = ((ModoJogo (moveJogador p Trepar),jogo),e)
evento (EventKey (SpecialKey KeyDown) Down _ _) ((ModoJogo p, jogo),e) = ((ModoJogo (moveJogador p InterageCaixa),jogo),e)
evento (EventKey (SpecialKey KeyEnter) Down _ _) ((VenceuJogo Fim , jogo),e) = (estadoinicial,e)
evento (EventKey (SpecialKey KeyEnter) Down _ _) ((VenceuJogo Continuar, [m1e1,m1e2,m1e3]),e) = ((ModoJogo m1e2, [m1e2,m1e3]),e) 
evento (EventKey (SpecialKey KeyEnter) Down _ _) ((VenceuJogo Continuar, [m1e2,m1e3]),e) = ((ModoJogo m1e3, [m1e3]),e) 
evento (EventKey (SpecialKey KeyDown) Down _ _) ((VenceuJogo Continuar, jogo),e) =((VenceuJogo Voltar, jogo),e)
evento (EventKey (SpecialKey KeyUp) Down _ _) ((VenceuJogo Continuar, jogo),e) = ((VenceuJogo Voltar, jogo),e)
evento (EventKey (SpecialKey KeyEnter) Down _ _) ((VenceuJogo Voltar, jogo),e) = (estadoinicial,e) 
evento (EventKey (SpecialKey KeyDown) Down _ _) ((VenceuJogo Voltar, jogo),e) =((VenceuJogo Continuar, jogo),e)
evento (EventKey (SpecialKey KeyUp) Down _ _) ((VenceuJogo Voltar, jogo),e) = ((VenceuJogo Continuar, jogo),e)
evento _ ((ModoJogo p , [m1e3]),e) | ganha p = ((VenceuJogo Fim, [m1e3]),e)
evento _ ((ModoJogo p, [m1e1,m1e2,m1e3]),e) | ganha p = ((VenceuJogo Continuar, [m1e1,m1e2,m1e3]),e)
evento _ ((ModoJogo p, [m1e2,m1e3]),e) | ganha p = ((VenceuJogo Continuar, [m1e2,m1e3]),e)
evento (EventKey (Char 'r') Down _ _) ((ModoJogo p, [m1e1,m1e2,m1e3] ),e ) = ((ModoJogo m1e1,[m1e1,m1e2,m1e3]),e)
evento (EventKey (Char 'r') Down _ _) ((ModoJogo p, [m1e2,m1e3] ),e ) = ((ModoJogo m1e2,[m1e2,m1e3]),e)
evento (EventKey (Char 'r') Down _ _) ((ModoJogo p, [m1e3] ),e ) = ((ModoJogo m1e3,[m1e3]),e)
evento _ w = w

{- | Função que reage um dado __WorldGloss__ conforme o tempo.

 -}

reagetempo :: Float -> WorldGloss -> WorldGloss
reagetempo _ w = w

{- | Função princial, que se encarrega de carregar as imagens e jogar o jogo.

 -}

main :: IO ()
main = do 
  persE <- loadBMP "personagemreduz.bmp"
  persO <- loadBMP "personagemreduzflip.bmp"
  caixa <- loadBMP "caixareduz.bmp"
  bloco <- loadBMP "chaoreduz.bmp"
  porta <- loadBMP "portareduz.bmp"
  play    
          window -- janela onde irá correr o jogo
          white -- cor do fundo da janela
          fr -- frame rate
          (estadoinicialgloss persE persO caixa bloco porta) -- estado inicial
          desenha -- desenha o estado do jogo
          evento -- reage a um evento
          reagetempo -- reage ao passar do tempo
