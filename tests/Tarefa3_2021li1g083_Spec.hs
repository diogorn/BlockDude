module Tarefa3_2021li1g083_Spec where

import Test.HUnit
import Tarefa3_2021li1g083
import Fixtures

testsT3 =
  test
    [ "Tarefa 3 - Teste Imprime Jogo m1e1" ~: "      <\n      X\n      X\nP   C X\nXXXXXXX" ~=?  show m1e1
    , "Tarefa 3 - Teste Imprime Jogo m1e2" ~: "       \n      X\n      X\nP < C X\nXXXXXXX" ~=?  show m1e2

    , "Tarefa 3 - Teste Imprime Jogo m1e2" ~: "     XXX    XXXXXXXXXX \n XXXX   XXXX          X\nX                     X\nX                     X\nX                     X\nX     X               X\nX     X               X\nX     XCCCC           X\nXP   XXXXXXX          X\nXX XXX     XX X      CX\n X X        X XX   CCCX\n X>X        X XX  CCCCX\n XXX        X XXXXXXXXX\n            XXX        " ~=?  show m2e1
    , "Tarefa 3 - Teste Imprime Jogo m1e2" ~: "     XXX    XXXXXXXXXX \n XXXX   XXXX          X\nX                     X\nX                     X\nX                     X\nX     X               X\nX     X               X\nX     XCCCC           X\nXP   XXXXXXX>         X\nXX XXX     XX X      CX\n X X        X XX   CCCX\n X X        X XX  CCCCX\n XXX        X XXXXXXXXX\n            XXX        " ~=?  show m2e2   
    
    ]
