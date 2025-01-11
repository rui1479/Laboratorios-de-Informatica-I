module Tarefa3_2021li1g078_Spec where

import Test.HUnit
import Tarefa3_2021li1g078
import Fixtures

testsT3 =
  test
    [ "Tarefa 3 - Teste Imprime Jogo m1e1" ~: "      <\n      X\n      X\nP   C X\nXXXXXXX" ~=?  show m1e1
    , "Tarefa 3 - Teste Imprime Jogo m1e2" ~: "       \n      X\n      X\nP < C X\nXXXXXXX" ~=?  show m1e2
    , "Tarefa 3 - Teste Imprime Jogo m1e3 (Jogador tem Caixa) " ~: "       \n      X\n   C  X\nP  >  X\nXXXXXXX" ~=?  show m1e3
    , "Tarefa 3 - Teste Imprime Jogo m1e4 (Jogador tem Caixa) " ~: "       \n      X\n     CX\nP    <X\nXXXXXXX" ~=?  show m1e4
    , "Tarefa 3 - Teste Imprime Jogo m2e1 (Jogador tem Caixa denivelada)" ~: "       \n      X\n C    X\nP>X   X\nXXXXXXX" ~=?  show m2e1
    , "Tarefa 3 - Teste Imprime Jogo m2e2 (Jogador tem Caixa denivelada)" ~: "       \n      X\n   C  X\nP X<  X\nXXXXXXX" ~=?  show m2e2
    , "Tarefa 3 - Teste Imprime Jogo m2e3 " ~: "       \n      X\n  C   X\nP X  >X\nXXXXXXX" ~=?  show m2e3
    ]