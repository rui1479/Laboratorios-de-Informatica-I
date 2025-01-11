module Fixtures where

import LI12122

m1 :: [(Peca, Coordenadas)]
m1 =
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Bloco, (4, 4)),
    (Caixa, (4, 3)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)),
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),
    (Bloco, (6, 1))
  ]

m1r :: Mapa
m1r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m2r :: Mapa
m2r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Caixa, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m3r :: Mapa
m3r = 
  [ [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Porta, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]


m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (6, 0) Oeste False)

m1e2 :: Jogo
m1e2 = Jogo m1r (Jogador (2, 3) Oeste False)

m1e3 :: Jogo
m1e3 = Jogo m1r (Jogador (3,3) Este True)

m1e4 :: Jogo
m1e4 = Jogo m1r (Jogador (5,3) Oeste True)

m1e5 :: Jogo
m1e5 = Jogo m1r (Jogador (5,3) Oeste False)

m1e6 :: Jogo
m1e6 = Jogo m1r (Jogador (0,3) Oeste False)

m2e1 :: Jogo
m2e1 = Jogo m2r (Jogador (1,3) Este True)

m2e2 :: Jogo
m2e2 = Jogo m2r (Jogador (3,3) Oeste True)

m2e3 :: Jogo
m2e3 = Jogo m2r (Jogador (5,3) Este False)

m2e4 :: Jogo
m2e4 = Jogo m2r (Jogador (2,1) Oeste False)

m3e1 :: Jogo
m3e1 = Jogo m3r (Jogador (16,4) Oeste False)


m2 :: [(Peca, Coordenadas)]
m2 =
  [ (Bloco, (0, 0)),
    (Bloco, (0, 1)),
    (Bloco, (0, 2)),
    (Bloco, (1,0 )),
    (Bloco, (1,2)),
    (Bloco, (2, 0)),
    (Bloco, (2, 1)),
    (Bloco, (2, 2)),
    (Porta, (1, 1))
  ]

m3 :: [(Peca, Coordenadas)]
m3 =
  [ (Vazio, (0, 0)),
    (Bloco, (0, 0)),
    (Bloco, (0, 2)),
    (Bloco, (1,0 )),
    (Bloco, (1,2)),
    (Bloco, (2, 0)),
    (Bloco, (2, 1)),
    (Bloco, (2, 2)),
    (Porta, (1, 1))
  ]

m4 :: [(Peca, Coordenadas)]
m4 =
  [ (Vazio, (0, 0)),
    (Bloco, (0, 1)),
    (Bloco, (0, 2)),
    (Bloco, (1,0 )),
    (Bloco, (1,2)),
    (Caixa, (2, 0)),
    (Vazio, (2, 1)),
    (Bloco, (2, 2)),
    (Porta, (1, 1))
  ]

m5 :: [(Peca, Coordenadas)]
m5 =
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (2, 4)),
    (Bloco, (6, 2)),
    (Bloco, (6, 1)),
    (Bloco, (1, 3)),
    (Bloco, (3, 3)),
    (Bloco, (3, 2)),
    (Bloco, (4, 2)),
    (Bloco, (5, 2)),
    (Bloco, (1, 4)),
    (Bloco, (3, 4))
  ]

m6 :: [(Peca, Coordenadas)]
m6 =
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (2, 4)),
    (Bloco, (6, 4)),
    (Bloco, (1, 3)),
    (Bloco, (3, 2)),
    (Bloco, (4, 4)),
    (Bloco, (5, 4)),
    (Bloco, (1, 4))
  ]