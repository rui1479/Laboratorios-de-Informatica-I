module Tarefa1_2021li1g078_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g078
import Fixtures

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1r" ~: True ~=? validaPotencialMapa m1
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: False ~=? validaPotencialMapa []
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: False ~=?  validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))]
    , "Tarefa 1 - Teste Valida Mapa com nenhum vazio" ~: False ~=? validaPotencialMapa m2
    , "Tarefa 1 - Teste Valida Mapa com duas coodenadas iguais" ~: False ~=? validaPotencialMapa m3
    , "Tarefa 1 - Teste Valida Mapa com caixa flutuante" ~: False ~=? validaPotencialMapa m4
    , "Tarefa 1 - Teste Valida Mapa com chão desregular, porém contínuo" ~: True ~=? validaPotencialMapa m5
    , "Tarefa 1 - Teste Valida Mapa com chão desregular, mas descontínuo" ~: False ~=? validaPotencialMapa m6
    ]
