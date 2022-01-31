module Tarefa1_2021li1g083_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g083
import Fixtures

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1r" ~: validaPotencialMapa m1 ~=? True
    , "Tarefa 1 - Teste Valida Mapa m1r" ~: validaPotencialMapa m2 ~=? True
    , "Tarefa 1 - Teste Valida Mapa caixa flutuante" ~: validaPotencialMapa m3 ~=? False
    , "Tarefa 1 - Teste Valida Mapa buraco no chao " ~: validaPotencialMapa m4 ~=? False
    , "Tarefa 1 - Teste Valida Mapa coordenadas repetidas " ~: validaPotencialMapa  [(Bloco, (0,0)), (Bloco, (0,0))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: validaPotencialMapa [] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))] ~=?  False
    ]
