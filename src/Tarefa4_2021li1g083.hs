{- |
Module      : Tarefa4_2021li1g083
Description : Movimentação do personagem
Copyright   : Diogo Neto <a98197@alunos.uminho.pt>;
            : Diogo Afonso <a101919@alunos.uminho.pt>;

__@Tarefa 4 do projeto de LI1 em 2021/22@__

Este módulo, referente à tarefa 4, contém definições Haskell relativas aos movimentos do /Jogador/
-}
module Tarefa4_2021li1g083 where

import LI12122
import Tarefa1_2021li1g083
import Tarefa2_2021li1g083
import Tarefa3_2021li1g083

{-| A função __"correrMovimentos"__ prepara uma série de movimentos para serem aplicados ao /Jogador/. 
-}
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo [] = jogo
correrMovimentos jogo (h:t) = correrMovimentos (moveJogador jogo h ) t
{-| __"moveJogador"__ é auxiliar de "correrMovimentos" e aplica um movimento especifico ao jogador. Esta função também identifica se o personagem carrega ou não uma /Caixa/.
-}
moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo mapa (Jogador (x,y) dir  b) )  movimento | movimento ==  AndarEsquerda && b == False = moveLeftFalse mapa (Jogador (x,y) dir  b) 
                                                           | movimento ==  AndarEsquerda && b  = moveLeftTrue mapa (Jogador (x,y) dir  b)
                                                           | movimento ==  AndarDireita  && b == False = moveRightFalse mapa (Jogador (x,y) dir  b)
                                                           | movimento ==  AndarDireita && b  = moveRightTrue mapa (Jogador (x,y) dir  b)
                                                           | movimento ==  Trepar  && b == False = moveUpFalse mapa (Jogador (x,y) dir  b)
                                                           | movimento ==  Trepar && b  = moveUpTrue mapa (Jogador (x,y) dir  b)
                                                           | movimento ==  InterageCaixa  && b == False = interCaixaFalse mapa (Jogador (x,y) dir  b)
                                                           | movimento ==  InterageCaixa && b  = interCaixaTrue mapa (Jogador (x,y) dir  b)



{-| A função __"checkPeca"__ identifica o tipo de peça presente numa coordenada especifica.
-}
checkPeca :: Mapa -> Coordenadas -> Coordenadas -> Peca
checkPeca ([]:_) _ _ = Bloco
checkPeca [] _ _ = Bloco
checkPeca ((h:hs):t) (x,y) (a,b) | y/=b = checkPeca t (x,y) (a,b+1)
                                 | x /= a =checkPeca (hs:t) (x,y) (a+1,b)
                                 | otherwise = h
{-| = Caso o jogador não carregue uma /Caixa/:
-}

{-| __"moveLeftFalse"__ é a função responsável por movimentar o jogador para a esquerda quando este não carrega uma caixa.
-}
moveLeftFalse :: Mapa -> Jogador -> Jogo
moveLeftFalse m  (Jogador (x,y) dir  b) | checkPeca m (x-1,y) (0,0) == Bloco || checkPeca m (x-1,y) (0,0) == Caixa = Jogo m (Jogador (x,y) Oeste b)
                                        | otherwise = Jogo m (gravidadeJogador ( desconstroiMapa m) (Jogador (x-1,y) Oeste b)) 

{-| __"moveRightFalse"__ movimenta o jogador para a direita quando este não carrega uma caixa.
-}
moveRightFalse :: Mapa -> Jogador -> Jogo
moveRightFalse m  (Jogador (x,y) dir  b) | checkPeca m (x+1,y) (0,0) == Bloco  || checkPeca m (x+1,y) (0,0) == Caixa = Jogo m (Jogador (x,y) Este b)
                                         | otherwise = Jogo m (gravidadeJogador ( desconstroiMapa m) (Jogador (x+1,y) Este b))

{-| __"moveUpFalse"__ permite ao jogador trepar /blocos/ ou /caixas/.
-}
moveUpFalse :: Mapa -> Jogador -> Jogo
moveUpFalse m  (Jogador (x,y) dir  b)   | dir == Oeste &&  checkPeca m (x-1,y) (0,0) /= Vazio && (checkPeca m (x-1,y-1) (0,0) == Vazio || checkPeca m (x-1,y-1) (0,0) == Porta) &&  checkPeca m (x-1,y) (0,0) /= Porta = Jogo m (Jogador (x-1,y-1) Oeste b)
                                        | dir == Este &&  checkPeca m (x+1,y) (0,0) /= Vazio && (checkPeca m (x+1,y-1) (0,0) == Vazio || checkPeca m (x+1,y-1) (0,0) == Porta) && checkPeca m (x+1,y) (0,0) /= Porta = Jogo m (Jogador (x+1,y-1) Este b)
                                        | otherwise = Jogo m (Jogador (x,y) dir b )

{-| __"interCaixaFalse"__ é responsável pela interação /Jogador/ - /Caixa/, ou seja, caso este interaja com uma caixa, passará a carregá-la se possivel.
-}
interCaixaFalse :: Mapa -> Jogador -> Jogo
interCaixaFalse m  (Jogador (x,y) dir  b) | dir == Oeste && checkPeca m (x-1,y) (0,0) == Caixa && checkPeca m (x,y-1) (0,0) == Vazio && checkPeca m (x-1,y-1)(0,0) == Vazio  = Jogo (moveCaixa m (x-1,y) (x,y-1)) (Jogador (x,y) dir True)
                                          | dir == Este && checkPeca m (x+1,y) (0,0) == Caixa && checkPeca m (x,y-1) (0,0) == Vazio && checkPeca m (x+1,y-1)(0,0) == Vazio =   Jogo (moveCaixa m (x+1,y) (x,y-1)) (Jogador (x,y) dir True)
                                          | otherwise = Jogo m (Jogador (x,y) dir False)


{-| = Caso o jogador carregue uma /Caixa/:
-}
{-| __"moveLeftTrue"__ é a função responsável por movimentar o jogador e a caixa que este carrega para a esquerda.
-}
moveLeftTrue :: Mapa -> Jogador -> Jogo
moveLeftTrue m  (Jogador (x,y) dir  b) | checkPeca m (x-1,y) (0,0) /= Vazio || checkPeca m (x-1,y-1) (0,0)/= Vazio = Jogo m (Jogador (x,y) Oeste b)
                                       | otherwise =Jogo (gravidadeCaixaTrue (desconstroiMapa ( moveCaixa m (x,y-1) (x-1,y-1)) ))(gravidadeJogador ( desconstroiMapa m) (Jogador (x-1,y) Oeste b))


{-| __"moveRightTrue"__ movimenta o jogador e a caixa que este carrega para a direita.
-}
moveRightTrue :: Mapa -> Jogador -> Jogo
moveRightTrue m  (Jogador (x,y) dir  b) | checkPeca m (x+1,y) (0,0) /= Vazio || checkPeca m (x+1,y-1) (0,0)/= Vazio  = Jogo m (Jogador (x,y) Este b) 
                                        | otherwise = Jogo (gravidadeCaixaTrue(desconstroiMapa (moveCaixa m (x,y-1) (x+1,y-1)))) (gravidadeJogador ( desconstroiMapa m) (Jogador (x+1,y) Este b))

{-| __"moveUpTrue"__ permite ao jogador trepar /blocos/ ou /caixas/ se estiver a carregar uma caixa.
-}
moveUpTrue :: Mapa -> Jogador -> Jogo
moveUpTrue m  (Jogador (x,y) dir  b)   | dir == Oeste &&  checkPeca m (x-1,y) (0,0) /= Vazio &&  checkPeca m (x-1,y) (0,0) /= Porta && checkPeca m (x-1,y-1) (0,0) == Vazio && checkPeca m (x-1,y-2) (0,0) == Vazio= Jogo (moveCaixa m (x,y-1) (x-1,y-2)) (Jogador (x-1,y-1) dir True)
                                       | dir == Este &&  checkPeca m (x+1,y) (0,0) /= Vazio &&  checkPeca m (x+1,y) (0,0) /= Porta && checkPeca m (x+1,y-1) (0,0) == Vazio && checkPeca m (x+1,y-2) (0,0) == Vazio= Jogo (moveCaixa m (x,y-1) (x+1,y-2)) (Jogador (x+1,y-1) dir True)
                                       | otherwise = Jogo m (Jogador (x,y) dir b )

{-| __"interCaixaTrue"__ é responsável pela interação /Jogador/ - /Caixa/, ou seja, como o jogador já carrega uma caixa, se este tentar apanhar outra, não irá conseguir.
-}
interCaixaTrue :: Mapa -> Jogador -> Jogo
interCaixaTrue  m (Jogador (x,y) dir  b) | dir == Oeste && checkPeca m (x-1,y-1) (0,0) == Vazio = Jogo (gravidadeCaixa(desconstroiMapa (moveCaixa m (x,y-1) (x-1,y-1)))) (Jogador (x,y) dir False)
                                         | dir == Este && checkPeca m (x+1,y-1) (0,0) == Vazio = Jogo (gravidadeCaixa(desconstroiMapa (moveCaixa m (x,y-1) (x+1,y-1)))) (Jogador (x,y) dir False)
                                         | otherwise = Jogo m (Jogador (x,y) dir  True)


{-| = Auxiliares
-}
{-| __"moveCaixa"__ é auxiliar das funções anteriores e permite executar os movimentos do jogador também na caixa para que ambos se movam corretamente
-}
moveCaixa :: Mapa -> Coordenadas -> Coordenadas -> Mapa 
moveCaixa m (x,y) (a,b) = addremoveCaixa (addremoveCaixa m (x,y) (0,0)) (a,b) (0,0)

{-| A __"addremoveCaixa"__ elimina ou adiciona uma caixa a uma coordenada sempre que um movimento/interação é executado, de modo a não existir a mesma caixa em posições diferentes
-}
addremoveCaixa :: Mapa -> Coordenadas -> Coordenadas -> Mapa
addremoveCaixa [] _ _ = []
addremoveCaixa  ((h:hs):t) (x,y) (a,b) | y/=b = (h:hs) : addremoveCaixa t (x,y) (a,b+1)
                                 |otherwise  = auxaddremoveCaixa (h:hs) (x,y) (a,b) : t
{-| __"auxaddremoveCaixa"__  é auxiliar da função anterior em que se detecta que uma peça é Caixa ou Vazia, se for Caixa substitui por Vazio, se for Vazio substitui por uma Caixa.
-}
auxaddremoveCaixa :: [Peca] ->  Coordenadas -> Coordenadas -> [Peca]
auxaddremoveCaixa (h:t) (x,y) (a,b) | x /= a = h: auxaddremoveCaixa t (x,y) (a+1,b)
                                    | h==Vazio = Caixa:t
                                    |otherwise = Vazio:t
{-| __"gravidadeCaixa"__ é responsável por colocar caixas flutuantes no chão sempre que o jogador as larga.
-}
gravidadeCaixa :: [(Peca, Coordenadas)]-> Mapa 
gravidadeCaixa m | checkBase m m == False = moveCaixa (constroiMapa m) (checkBase1 m m) (findFloor m (checkBase1 m m))
                 | otherwise = constroiMapa m
{-| __"gravidadeCaixaTrue"__ é identica a anterior no entanto é usada para quando o Jogador está a segurar a Caixa.
-}
gravidadeCaixaTrue :: [(Peca, Coordenadas)]-> Mapa 
gravidadeCaixaTrue m | checkBase m m == False = moveCaixa (constroiMapa m) (checkBase1 m m) (findFloorTrue m (checkBase1 m m))
                     | otherwise = constroiMapa m


{-| __"gravidadeJogador"__ permite ao jogador 'cair', ou seja, sempre que o jogador passe para um nivel mais baixo do que se encontra, a "gravidadeJogador" coloca-o no chão para que este não flutue no mapa
-}    
gravidadeJogador :: [(Peca, Coordenadas)] -> Jogador -> Jogador 
gravidadeJogador m (Jogador (x,y) dir b )= Jogador (findFloor m (x,y)) dir b

{-| __"checkBase1"__ verifica se a base de uma caixa é outra caixa ou um bloco, função alternativa a da tarefa 1, esta retorna as coordenadas onde a caixa flutuante se encontra, ao contrario da original, que retorna um Bool
-}
checkBase1 :: [(Peca, Coordenadas)]-> [(Peca, Coordenadas)] -> Coordenadas
checkBase1 ((p,(x,y)):t) l | p==Caixa && checkBloco (x,y+1) l == False = (x,y)
                           | otherwise = checkBase1 t l

{-| __"findFloor"__ é uma auxiliar e permite à "gravidadeCaixa" achar onde se encontra o chão mais proximo para lá colocar o jogador.
-}
findFloor :: [(Peca, Coordenadas)] -> Coordenadas -> Coordenadas
findFloor ((p,(x,y)):t) (a,b) | x == a && y /= b && y>= b && (p==Bloco || p == Caixa )= (x,y-1)
                              | otherwise = findFloor t (a,b)
{-| __"findFloorTrue"__ é uma auxiliar e permite à "gravidadeCaixaTrue" achar onde se encontra o chão mais proximo (na coordenada acima do Jogador) para lá colocar o jogador e a caixa que este carrega.
-}
findFloorTrue :: [(Peca, Coordenadas)] -> Coordenadas -> Coordenadas
findFloorTrue ((p,(x,y)):t) (a,b) | x == a && y > b && (p==Bloco || p == Caixa )= (x,y-2)
                              | otherwise = findFloorTrue t (a,b)                      
