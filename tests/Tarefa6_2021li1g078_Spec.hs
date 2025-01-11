module Tarefa6_2021li1g078_Spec where

import Test.HUnit
import Tarefa6_2021li1g078
import Fixtures
import LI12122

testsT6 =
  test
    [ " Tarefa 6 - Resolve jogo m1e5 num maximo de 5 movimentos" ~: Just [Trepar, AndarEsquerda, AndarEsquerda, AndarEsquerda, AndarEsquerda] ~=? resolveJogo 5 m1e5
    , " Tarefa 6 - Resolve jogo m1e5 num maximo de 4 movimentos" ~: Nothing ~=? resolveJogo 4 m1e5
    , " Tarefa 6 - Resolve jogo m1e5 num maximo de 0 movimentos" ~: Nothing ~=? resolveJogo 0 m1e5
    , " Tarefa 6 - Resolve jogo m2e4 num maximo de 2 movimentos" ~: Just [AndarEsquerda, AndarEsquerda] ~=? resolveJogo 2 m2e4
    , " Tarefa 6 - Resolve jogo m2e4 num maximo de 1 movimento" ~: Nothing ~=? resolveJogo 1 m2e4
    , " Tarefa 6 - Resolve jogo m2e4 num maximo de 0 movimentos" ~: Nothing ~=? resolveJogo 0 m2e4
    , " Tarefa 6 - Resolve jogo m1e6 num maximo de 0 movimento" ~: Just [] ~=? resolveJogo 0 m1e6
    , " Tarefa 6 - Resolve jogo m3e1 num maximo de 19 movimentos" ~: Just [AndarEsquerda,InterageCaixa,AndarEsquerda,InterageCaixa,Trepar,Trepar,AndarEsquerda,InterageCaixa,AndarEsquerda,AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,InterageCaixa,Trepar,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda] ~=? resolveJogo 19 m3e1
    , " Tarefa 6 - Resolve jogo m3e1 num maximo de 18 movimentos" ~: Nothing ~=? resolveJogo 18 m3e1
    , " Tarefa 6 - Resolve jogo m3e1 num maximo de 18 movimentos" ~: Nothing ~=? resolveJogo 0 m3e1
    ]