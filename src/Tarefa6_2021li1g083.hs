{- |
Module      : Tarefa6_2021li1g083
Description : Resolução de um puzzle
Copyright   : Diogo Neto <a98197@alunos.uminho.pt>;
            : Diogo Afonso <a101919@alunos.uminho.pt>;

__@Tarefa 6 do projeto de LI1 em 2021/22@__    

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}
module Tarefa6_2021li1g083 where

import LI12122
import Tarefa1_2021li1g083
import Tarefa2_2021li1g083  
import Tarefa3_2021li1g083  
import Tarefa4_2021li1g083
import Tarefa5_2021li1g083
import Levels

-- | = Bot
-- A tarefa não estar terminada pois não encontramos um algoritmo eficiente para a resolução de problemas que envolve a iteração com múltiplas caixas, então decidimos fazer com que o "Bot" nao iteraja com elas

-- | direção que o bot pode tomar
data Where2go 
   = Direita -- ^ direita
   | Esquerda -- ^ esquerda
   | Chegou -- ^ Fim do nível
   deriving (Eq, Show)


-- | __resolveJogo__ tenta resolver o mapa
resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo x j | x < length (moveBotList j) = Nothing
                | checkChegada (correrMovimentos j (moveBotList j)) = Just (moveBotList j) 
                | otherwise = Nothing
  
-- | __findDoor__ percorre o mapa à procura das coordenadas da porta
findDoor :: [(Peca, Coordenadas)] -> Coordenadas
findDoor ((p,c):t) | p == Porta = c
                   | otherwise = findDoor t

-- | __direcao__ indica que direção o bot deve tomar ou se já chegou
direcao :: Jogador -> Coordenadas -> Where2go
direcao (Jogador (x,y) _ _ ) (a,b) | x < a = Direita
                                  | x > a = Esquerda
                                  | otherwise = Chegou

-- | __moveBot__ é auxiliar da @moveBotList@
moveBot :: Jogo -> (Maybe Movimento,Maybe Where2go)
moveBot (Jogo m (Jogador (x,y) dir b))  | direcao (Jogador (x,y) dir b) (findDoor (desconstroiMapa m)) == Direita && (checkPeca m (x+1,y) (0,0)== Bloco || checkPeca m (x+1,y) (0,0)== Caixa) && (checkPeca m (x+1,y-1) (0,0) /= Bloco && checkPeca m (x+1,y-1) (0,0) /= Caixa)= (Just Trepar,Just Direita)
                                        | direcao (Jogador (x,y) dir b) (findDoor (desconstroiMapa m)) == Direita && (checkPeca m (x+1,y) (0,0)/= Bloco && checkPeca m (x+1,y) (0,0)/= Caixa) = (Just AndarDireita,Just Direita)
                                        | direcao (Jogador (x,y) dir b) (findDoor (desconstroiMapa m)) == Esquerda && (checkPeca m (x-1,y) (0,0)== Bloco || checkPeca m (x-1,y) (0,0)== Caixa) && (checkPeca m (x-1,y-1) (0,0) /= Bloco && checkPeca m (x-1,y-1) (0,0) /= Caixa)= (Just Trepar,Just Esquerda)
                                        | direcao (Jogador (x,y) dir b) (findDoor (desconstroiMapa m)) == Esquerda && (checkPeca m (x-1,y) (0,0)/= Bloco && checkPeca m (x-1,y) (0,0)/= Caixa) = (Just AndarEsquerda, Just Esquerda)
                                        | otherwise = (Nothing,Nothing)

-- | __moveBotList__ executa a lista de movimentos no bot
moveBotList :: Jogo -> [Movimento]
moveBotList (Jogo m (Jogador (x,y) dir b))   | moveBot (Jogo m (Jogador (x,y) dir b)) == (Just AndarDireita,Just Direita) = AndarDireita : moveBotList (Jogo m (gravidadeJogador (desconstroiMapa m) (Jogador (x+1,y) dir b)))
                                             | moveBot (Jogo m (Jogador (x,y) dir b)) == (Just Trepar,Just Direita) = Trepar : moveBotList (Jogo m (gravidadeJogador (desconstroiMapa m) (Jogador (x+1,y-1) dir b)))
                                             | moveBot (Jogo m (Jogador (x,y) dir b)) == (Just AndarEsquerda,Just Esquerda) = AndarEsquerda : moveBotList (Jogo m (gravidadeJogador (desconstroiMapa m) (Jogador (x-1,y) dir b)))
                                             | moveBot (Jogo m (Jogador (x,y) dir b)) == (Just Trepar,Just Esquerda) = Trepar : moveBotList (Jogo m (gravidadeJogador (desconstroiMapa m) (Jogador (x-1,y-1) dir b)))
                                             | otherwise = []

-- | __checkChegada__ verifica se o bot chegou ou não ao fim do nível
checkChegada :: Jogo -> Bool
checkChegada  (Jogo m (Jogador (x,y) dir b)) | findDoor(desconstroiMapa m) == (x,y) = True 
                                             | otherwise = False