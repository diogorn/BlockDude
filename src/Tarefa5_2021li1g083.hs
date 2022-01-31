{- |
Module      : Tarefa5_2021li1g083 aka Main
Description : Aplicaçcão gráfica completa
Copyright   : Diogo Neto <a98197@alunos.uminho.pt>;
            : Diogo Afonso <a101919@alunos.uminho.pt>;

__@Tarefa 5 do projeto de LI1 em 2021/22@__

Este módulo, o principal, referente à tarefa 5, contém definições Haskell relativas à representação do /Jogo/
-}
module Tarefa5_2021li1g083 where -- main module

import LI12122
import Tarefa1_2021li1g083
import Tarefa2_2021li1g083
import Tarefa4_2021li1g083
import Tarefa3_2021li1g083
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game
import Data.List (elemIndex)
import Data.Maybe
import Levels
import GHC.Base (Float)
import LI12122 (Movimento(InterageCaixa, AndarDireita, AndarEsquerda))

-- | __"Menus"__ do /Jogo/.

data Menu
   = MainMenu -- ^ Menu principal
   | GameMenu -- ^ Menu para iniciar novo jogo
   | MapMenu -- ^ Menu de seleção dos mapas
   | BetweenLevels -- ^ Menu entre níveis 
   | Play -- ^ Opção em @BetweenLevels@ que permite jogar o nivel selecionado.
   | MapEditor -- ^ Menu que permite ao jogador construir um mapa personalizado (sub-tarefa incompleta)
   | CreditsMenu -- ^ Menu que apresenta os Créditos do /Jogo/
  deriving (Eq, Show)


-- | Opções dentro dos menus.
data Options 
  = Mapa1 -- ^ Nível 1
  | Mapa2 -- ^ Nível 2
  | Mapa3 -- ^ Nível 3
  | Mapa4 -- ^ Nível 4
  | MapaB -- ^ Nível Bónus que só é possível acessar passando o nível 4, __"Easter egg"__.
  deriving (Eq, Show, Read)


-- | Definições da janela do jogo
mainDisplay :: Display
mainDisplay = InWindow "BlockDude" displayDimension (0,0)

-- | __"displayDimension"__ atribui uma dimensão à janela do jogo
displayDimension :: (Int,Int)
displayDimension = (1250,640)

type InGame = (Mapa,Jogador)
type Estado = Menu

-- estado inicial, semelhante ao anterior mas duas imagens 
-- e um valor de segundos passados desde o inicio do programa
type EstadoGloss = (Estado,Options,InGame, [Picture],[Picture],Peca)


-- | __"estadoGlossInicial"__ define o estado inicial do gloss 
estadoGlossInicial :: [Picture] -> [Picture] -> EstadoGloss
estadoGlossInicial [mainBg,mi,mj,sl,ed,bl,cr] [gameBG, jR, jRt, jL, jLt, v, c, b, p] = (MainMenu,Mapa1,(m1,j1), [mainBg,mi,mj,sl,ed,bl,cr], [gameBG, jR, jRt, jL, jLt, v, c, b, p], Vazio)

-- | __"doMovimento"__ executa movimentos no /Jogo/
doMovimento :: InGame -> Movimento -> Jogo
doMovimento (m,j) movimento = moveJogador (Jogo m j)  movimento

-- | __"jogoToEstado"__ troca o tipo do Jogo para InGame
jogoToEstado :: Jogo -> InGame 
jogoToEstado (Jogo mapa jogador) = (mapa,jogador)


-- | __"reageEventoGloss"__ é a função que executa ações no jogo com base no menu em que o jogador se encontra e com base na opção escolhida
reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss 
reageEventoGloss _ (Play,Mapa1,ig,[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], Porta) = (BetweenLevels,Mapa1,ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], Vazio)
reageEventoGloss _ (Play,Mapa2,ig,[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], Porta) = (BetweenLevels,Mapa2,ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], Vazio)
reageEventoGloss _ (Play,Mapa3,ig,[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], Porta) = (BetweenLevels,Mapa3,ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], Vazio)
reageEventoGloss _ (Play,Mapa4,ig,[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], Porta) = (BetweenLevels,Mapa4,ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], Vazio)
reageEventoGloss _ (Play,MapaB,ig,[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], Porta) = (CreditsMenu,MapaB, ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], Vazio)


-- menu between_levels
reageEventoGloss (EventKey (Char 'n') Down _ _) (BetweenLevels,op,ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) = switchNiveis  (BetweenLevels,op,ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) True
reageEventoGloss (EventKey (Char 'm') Down _ _) (BetweenLevels,op,ig,[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) = (MainMenu,op, ig,[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t)
reageEventoGloss (EventKey (Char 'p') Down _ _) (BetweenLevels,op,ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) = switchNiveis  (BetweenLevels,op,ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) False -- play again aka reset
--reageEventoGloss (EventKey (Char 's') Down _ _) (BetweenLevels,op,ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) = --save game para ser usado no resume


-- dentro dos mapas
reageEventoGloss (EventKey (Char 'm') Down _ _) (Play,op,ig,[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) = (MainMenu,op, ig,[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t)
reageEventoGloss (EventKey (Char 'r') Down _ _) (Play,op,ig,[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) = switchNiveis  (BetweenLevels,op,ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) False -- reset do nivel
reageEventoGloss (EventKey (Char 'c') Down _ _) (Play,op,ig,[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) = switchNiveis  (BetweenLevels,op,ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) True -- cheat para saltar niveis facilmente

reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _)     (Play,op,(mapa,Jogador (x,y) dir boo),im,[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) = (Play,op,jogoToEstado(doMovimento (mapa,Jogador (x,y) dir boo) Trepar),im, [gameBG, jR, jRt, jL, jLt, v, c, b, p], checkPeca mapa (x,y) (0,0))
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _)   (Play,op,(mapa,Jogador (x,y) dir boo),im,[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) = (Play,op,jogoToEstado(doMovimento (mapa,Jogador (x,y) dir boo) InterageCaixa),im, [gameBG, jR, jRt, jL, jLt, v, c, b, p], checkPeca mapa (x,y) (0,0))   
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _)   (Play,op,(mapa,Jogador (x,y) dir boo),im,[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) = (Play,op,jogoToEstado(doMovimento (mapa,Jogador (x,y) dir boo) AndarEsquerda),im, [gameBG, jR, jRt, jL, jLt, v, c, b, p], checkPeca mapa (x,y) (0,0)) 
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _)  (Play,op,(mapa,Jogador (x,y) dir boo),im,[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) = (Play,op,jogoToEstado(doMovimento (mapa,Jogador (x,y) dir boo) AndarDireita),im, [gameBG, jR, jRt, jL, jLt, v, c, b, p], checkPeca mapa (x,y) (0,0)) 


-- menu inicial 
reageEventoGloss (EventKey (Char 'p') Down _ _) (MainMenu,op, ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) = (GameMenu,op, ig,[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t)
reageEventoGloss (EventKey (Char 'c') Down _ _) (MainMenu,op, ig, [mainBg,mi,mj,sl,ed,bl,cr],ij, t) = (CreditsMenu,op, ig,[mainBg,mi,mj,sl,ed,bl,cr],ij, t)


-- menu jogar
reageEventoGloss (EventKey (Char 'n') Down _ _) (GameMenu,op, ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) = (MapMenu,op, ig,[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t)
reageEventoGloss (EventKey (Char 'm') Down _ _) (GameMenu,op, ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) = (MainMenu,op, ig,[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t)


-- menu select levels
reageEventoGloss (EventKey (Char '1') Down _ _) (MapMenu,op, ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) = (Play,Mapa1,(m1,j1),[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t)
reageEventoGloss (EventKey (Char '2') Down _ _) (MapMenu,op, ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) = (Play,Mapa2,(m2,j2),[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t)
reageEventoGloss (EventKey (Char '3') Down _ _) (MapMenu,op, ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) = (Play,Mapa3,(m3,j3),[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t)
reageEventoGloss (EventKey (Char '4') Down _ _) (MapMenu,op, ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) = (Play,Mapa4,(m4,j4),[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t)
reageEventoGloss (EventKey (Char '0') Down _ _) (MapMenu,op, ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) = (Play,MapaB,(mBonus,jBonus),[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) -- tecla que permite acessar o nivel bónus sem ter que passar pelo nivel 4
reageEventoGloss (EventKey (Char 'm') Down _ _) (MapMenu,op, ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) = (MainMenu,op, ig,[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t)
reageEventoGloss (EventKey (Char '+') Down _ _) (MapMenu,op, ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) = (MapEditor,op,ig,[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t)


-- menu editor
reageEventoGloss (EventKey (Char 'm') Down _ _) (MapEditor,op, ig,[mainBg,mi,mj,sl,ed,bl,cr],[jR, jRt, jL, jLt, v, c, b, p], t)  =  (MainMenu,op, ig,[mi,mj,sl,ed,bl,cr],[jR, jRt, jL, jLt, v, c, b, p], t)

-- creditos
reageEventoGloss _ (CreditsMenu,op, ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) =  (MainMenu,op, ig,[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t)
reageEventoGloss _ s = s -- ignora qualquer outro evento 

-- | __"switchNiveis"__ executa o mapa seguinte se o jogador concluir o nível ou dá reset/play again se o jogador quiser
switchNiveis :: EstadoGloss -> Bool -> EstadoGloss 
switchNiveis (BetweenLevels,op,ig, [mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], t) bool | bool = case op of
                                                                                                                 Mapa1 -> (Play,Mapa2,(m2,j2),[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], Vazio)
                                                                                                                 Mapa2 -> (Play,Mapa3,(m3,j3),[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], Vazio)
                                                                                                                 Mapa3 -> (Play,Mapa4,(m4,j4),[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], Vazio)
                                                                                                                 Mapa4 -> (Play,MapaB,(mBonus,jBonus),[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], Vazio)
                                                                                                                 otherwise -> (CreditsMenu,Mapa1, (m1,j1),[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], Vazio)
                                                                                                              | otherwise = case op of
                                                                                                                 Mapa1 -> (Play,Mapa1,(m1,j1),[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], Vazio)
                                                                                                                 Mapa2 -> (Play,Mapa2,(m2,j2),[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], Vazio)
                                                                                                                 Mapa3 -> (Play,Mapa3,(m3,j3),[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], Vazio)
                                                                                                                 Mapa4 -> (Play,Mapa4,(m4,j4),[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], Vazio)
                                                                                                                 MapaB -> (Play,MapaB,(mBonus,jBonus),[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], Vazio)
--                                                                                                               otherwise -> (GameMenu,Mapa1, (m1,j1),[mainBg,mi,mj,sl,ed,bl,cr],[gameBG, jR, jRt, jL, jLt, v, c, b, p], Vazio) -- nunca vai ser usado mas volta ao menu em caso de erro


-- | __"reagetempoGloss"__ O intuíto da função era adicionar alguma ação ao passar do tempo mas acabamos por não o fazer, então retorna exatamente o estado anterior
reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss 
reageTempoGloss n (a , b , c, d,e, t) = (a , b ,c, d,e,t)


-- | __"desenhaEstadoGloss"__ atribui uma imagem gráfica a cada menu definido ns struct @Menu@
desenhaEstadoGloss :: EstadoGloss -> Picture 
desenhaEstadoGloss (Play,_, (m,j), im, [gameBG, jR, jRt, jL, jLt, v, c, b, p],_) = pictures ([gameBG] ++ desenhaMapa m j 0 m [gameBG, jR, jRt, jL, jLt, v, c, b, p])
desenhaEstadoGloss (MainMenu,_, (m,j), [mainBg,mi,mj,sl,ed,bl,cr] ,im,t) = pictures ([mainBg,mi])
desenhaEstadoGloss (GameMenu,_, (m,j), [mainBg,mi,mj,sl,ed,bl,cr] ,im,t) = pictures ([mainBg,mj])
desenhaEstadoGloss (MapMenu,_, (m,j), [mainBg,mi,mj,sl,ed,bl,cr] ,im,t) = pictures ([mainBg,sl])
desenhaEstadoGloss (MapEditor,_, (m,j), [mainBg,mi,mj,sl,ed,bl,cr] ,im,t) = pictures ([mainBg,ed])
desenhaEstadoGloss (BetweenLevels,_, (m,j), [mainBg,mi,mj,sl,ed,bl,cr] ,im,t) =pictures ([mainBg,bl])
desenhaEstadoGloss (CreditsMenu,_, (m,j), [mainBg,mi,mj,sl,ed,bl,cr] ,im,t) = pictures ([mainBg,cr])


-- | Definição dos frames por segundo, isto é, o número de vezes que a função reageTempoGloss é executada num segundo
fps :: Int
fps = 45


-- | A função __"main"__ importa as imagens e corre o jogo.
main :: IO ()
main = do
    Just jogadorR       <- loadJuicyPNG "imagens/charright.png"
    Just jogadorL       <- loadJuicyPNG "imagens/charleft.png"
    Just jogadorRtrue   <- loadJuicyPNG "imagens/charrightTrue.png"
    Just jogadorLtrue   <- loadJuicyPNG "imagens/charleftTrue.png"
    Just v              <- loadJuicyPNG "imagens/vazio.png"
    Just c              <- loadJuicyPNG "imagens/caixa.png"
    Just b              <- loadJuicyPNG "imagens/bloco.png"
    Just p              <- loadJuicyPNG "imagens/porta.png"
    Just mi             <- loadJuicyPNG "imagens/menus/menu_inicial.png"
    Just mj             <- loadJuicyPNG "imagens/menus/menu_jogar.png"
    Just sl             <- loadJuicyPNG "imagens/menus/select_levels.png"
    Just ed             <- loadJuicyPNG "imagens/menus/editor.png"
    Just bl             <- loadJuicyPNG "imagens/menus/between_levels.png"
    Just cr             <- loadJuicyPNG "imagens/menus/creditos.png"
    Just gameBG         <- loadJuicyPNG "imagens/menus/game_background.png"
    Just mainBg         <- loadJuicyPNG "imagens/menus/background.png"
    
    play mainDisplay
         (greyN 0.25)
         fps 
         (estadoGlossInicial [mainBg,mi,mj,sl,ed,bl,cr] [gameBG, jogadorR, jogadorRtrue, jogadorL, jogadorLtrue , v, c, b, p])
         desenhaEstadoGloss 
         reageEventoGloss
         reageTempoGloss

-- | __"sc"__ redimensiona os elementos do tipo Picture para a escala do jogo
sc :: Mapa -> Picture -> Picture          
sc m = Scale (gameScale m) (gameScale m)

-- | __"gameScale"__define a escala do jogo
gameScale :: Mapa -> Float
gameScale m = 18/fromIntegral (max (l + 1) (r - 5))
    where (l,r) = indiceMatriz (jCoordenadas(desconstroiMapa m))

-- | __"pieceSide"__ define o tamanho que cada @Peca@ deve ter na janela do jogo
pieceSide :: Mapa -> Float
pieceSide m = 32 * gameScale m

-- | __"desenhaMapa"__ desenha o jogo 
desenhaMapa :: Mapa -> Jogador -> Int -> Mapa -> [Picture] -> [Picture]
desenhaMapa [] _ _ _ _ = []
desenhaMapa (l:ls) (Jogador (x,y) dir b) n mapa pics = desenhaLinha l (n,0) (Jogador (x,y) dir b) mapa pics ++ desenhaMapa ls (Jogador (x,y) dir b) (n + 1) mapa pics

-- | __"desenhaLinha"__ é auxiliar da @desenhaMapa@ e desenha cada linha de um 'Mapa' na janela do gloss.
desenhaLinha :: [Peca] -> Coordenadas -> Jogador -> Mapa -> [Picture] -> [Picture]
desenhaLinha [] _ _ _ _ = []
desenhaLinha (h:t) (n,x) (Jogador (xs,ys) dir boo) mapa [gameBG, jR, jRt, jL, jLt, v, c, b, p] | (x,n) == (xs,ys) && dir == Este && boo == False = Translate posPecaX posPecaY (sc mapa jR) : desenhaLinha t (n,x + 1) (Jogador (xs,ys) dir boo) mapa [gameBG, jR, jRt, jL, jLt, v, c, b, p]
                                                                                               | (x,n) == (xs,ys) && dir == Este && boo == True = Translate posPecaX posPecaY (sc mapa jRt) : desenhaLinha t (n,x + 1) (Jogador (xs,ys) dir boo) mapa [gameBG, jR, jRt, jL, jLt, v, c, b, p]
                                                                                               | (x,n) == (xs,ys) && dir == Oeste && boo == False = Translate posPecaX posPecaY (sc mapa jL) : desenhaLinha t (n,x + 1) (Jogador (xs,ys) dir boo) mapa [gameBG, jR, jRt, jL, jLt, v, c, b, p]
                                                                                               | (x,n) == (xs,ys) && dir == Oeste && boo == True = Translate posPecaX posPecaY (sc mapa jLt) : desenhaLinha t (n,x + 1) (Jogador (xs,ys) dir boo) mapa [gameBG, jR, jRt, jL, jLt, v, c, b, p]
                                                                                               | h == Vazio = Translate posPecaX posPecaY (sc mapa v) : desenhaLinha t (n,x + 1) (Jogador (xs,ys) dir boo) mapa [gameBG, jR, jRt, jL, jLt, v, c, b, p]
                                                                                               | h == Bloco = Translate posPecaX posPecaY (sc mapa b) : desenhaLinha t (n,x + 1) (Jogador (xs,ys) dir boo) mapa [gameBG, jR, jRt, jL, jLt, v, c, b, p]
                                                                                               | h == Porta = Translate posPecaX posPecaY (sc mapa p) : desenhaLinha t (n,x + 1) (Jogador (xs,ys) dir boo) mapa [gameBG, jR, jRt, jL, jLt, v, c, b, p]
                                                                                               | h == Caixa = Translate posPecaX posPecaY (sc mapa c) : desenhaLinha t (n,x + 1) (Jogador (xs,ys) dir boo) mapa [gameBG, jR, jRt, jL, jLt, v, c, b, p]
                                    
    where posPecaX = (fromIntegral x - (fromIntegral mapRows/2) + oddAdj) * pieceSide mapa
          posPecaY = ((fromIntegral mapLines/2) - fromIntegral n) * pieceSide mapa
          (mapLines,mapRows) = indiceMatriz (jCoordenadas(desconstroiMapa mapa)) 
          oddAdj | odd mapRows = 0.5
                 | otherwise = 0



