module Tarefa2_2021li1g083_Spec where

import Data.List (sort)
import Test.HUnit
import LI12122
import Tarefa2_2021li1g083
import Fixtures

testsT2 =
  test
    [ "Tarefa 2 - Teste Construir Mapa m1" ~: m1r ~=? constroiMapa m1
    , "Tarefa 2 - Teste Construir Mapa m1" ~: m2r ~=? constroiMapa m2
    , "Tarefa 2 - Teste Construir Mapa m1" ~: m3r ~=? constroiMapa m3
    , "Tarefa 2 - Teste Construir Mapa m1" ~: m4r ~=? constroiMapa m4
    , "Tarefa 2 - Teste Construir Mapa vazio" ~: [] ~=? constroiMapa []
    , "Tarefa 2 - Teste Desconstruir Mapa m1" ~: sort m1 ~=?  sort (desconstroiMapa m1r)
    , "Tarefa 2 - Teste Desconstruir Mapa m1" ~: sort m2 ~=?  sort (desconstroiMapa m2r)
    , "Tarefa 2 - Teste Desconstruir Mapa m1" ~: sort m3 ~=?  sort (desconstroiMapa m3r)
    , "Tarefa 2 - Teste Desconstruir Mapa m1" ~: sort m4 ~=?  sort (desconstroiMapa m4r)
    , "Tarefa 2 - Teste Desconstruir Mapa vazio" ~: [] ~=? desconstroiMapa []
    , "Tarefa 2 - Teste Identidade m1" ~: sort m1 ~=?  sort (desconstroiMapa (constroiMapa m1))
    , "Tarefa 2 - Teste Identidade m1" ~: sort m2 ~=?  sort (desconstroiMapa (constroiMapa m2))
    , "Tarefa 2 - Teste Identidade m1" ~: sort m3 ~=?  sort (desconstroiMapa (constroiMapa m3))
    , "Tarefa 2 - Teste Identidade m1" ~: sort m4 ~=?  sort (desconstroiMapa (constroiMapa m4))
    , "Tarefa 2 - Teste Identidade m1r" ~: m1r ~=?  constroiMapa (desconstroiMapa m1r)
    , "Tarefa 2 - Teste Identidade m1r" ~: m2r ~=?  constroiMapa (desconstroiMapa m2r)
    , "Tarefa 2 - Teste Identidade m1r" ~: m3r ~=?  constroiMapa (desconstroiMapa m3r)
    , "Tarefa 2 - Teste Identidade m1r" ~: m4r ~=?  constroiMapa (desconstroiMapa m4r)
    , "Tarefa 2 - Teste Construir Sobrepor Peças" ~: constroiMapa [(Porta, (7, 4))] ~=?  constroiMapa [(Porta, (7, 4)), (Porta, (7, 4))]
    ]