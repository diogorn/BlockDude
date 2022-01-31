{- |
Module      : Tarefa3_2021li1g083
Description : Representação textual do jogo
Copyright   : Diogo Neto <a98197@alunos.uminho.pt>;
            : Diogo Afonso <a101919@alunos.uminho.pt>;

__@Tarefa 3 do projeto de LI1 em 2021/22@__

Este módulo, referente à tarefa 3, contém definições Haskell para o output do mapa
-}
module Tarefa3_2021li1g083 where
import Tarefa1_2021li1g083
import Tarefa2_2021li1g083 
import LI12122

instance Show Jogo where
  show (Jogo m (Jogador (x,y) dir b)) = nconcat (addJogador (showMapa m ) (Jogador (x,y) dir b) (0,0))

{-| __"showMapa"__ reúne os elementos do tipo /Mapa/ para serem interpretados por "mostra"
-}
showMapa :: Mapa -> [String]
showMapa mapa = map (concatMap desenha) mapa 

{-| A função __"desenha"__ associa a cada um dos elementos do tipo /Peca/ contidos na lista (Peca,Coordenadas) num caracter, colocando-os seguidos numa string para serem interpretados pelo "showMapa"  
-}
desenha ::  Peca -> String
desenha p = case p of
            Vazio ->  " "
            Caixa ->  "C"
            Bloco ->  "X"
            Porta ->  "P"

{-| __"nconcat"__ separa as linhas da matriz correspondente ao mapa por um caracter "\n" que representa a respetiva mudança no terminal.
-}
nconcat :: [String] -> String
nconcat [l] | length [l] == 1 = l
nconcat (h:t) = h ++ "\n" ++ nconcat t

{-| __"auxaddJogador"__ atribui um caracter consoante a direção do jogador de modo a ser facilmente identificável visualmente no terminal 
-}
auxaddJogador :: Jogador -> String
auxaddJogador (Jogador (a,b) dir c) | dir == Este = ">"
                                    | otherwise = "<"
{-| A função __"addJogador"__ percorre todas as colunas do mapa e coloca o jogador na respetiva posição, com a ajuda de "auxaddJogador" coloca-o virado para a direção correta.
  
-}
addJogador :: [String] -> Jogador -> Coordenadas -> [String]
addJogador [] (Jogador (a,b) dir bo) (c,d) = []
addJogador ((x:xs):ys) (Jogador (a,b) dir bo) (c,d)| b /= d = (x:xs) : addJogador ys (Jogador (a,b) dir bo) (c,d+1)
                                                   |otherwise = auxJogador (x:xs) (Jogador (a,b) dir bo) (c,d) : addJogador ys (Jogador (a,b) dir bo) (c,d+1)
{-| __"auxJogador"__ é auxiliar da "addJogador" e percorre todas as linhas do mapa e coloca o jogador na respetiva posição
-}
auxJogador :: String -> Jogador -> Coordenadas -> String
auxJogador (x:xs) (Jogador (a,b) dir bo) (c,d) |a /= c  = x : auxJogador xs (Jogador (a,b) dir bo) (c+1,d)
                                               | otherwise = auxaddJogador (Jogador (a,b) dir bo) ++ xs
