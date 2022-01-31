{- |
Module      : Tarefa2_2021li1g083
Description : Construção/Desconstrução do mapa
Copyright   : Diogo Neto <a98197@alunos.uminho.pt>;
            : Diogo Afonso <a101919@alunos.uminho.pt>;

__@Tarefa 2 do projeto de LI1 em 2021/22@__

Este módulo, referente à tarefa 2, contém definições Haskell para a construção/desconstrução de um mapa válido já avaliado na Tarefa 1
-}
module Tarefa2_2021li1g083 where
import LI12122
import Tarefa1_2021li1g083

import Data.Ix
{-| = Funções relativas à desconstrução do mapa
-}
{-| __"desconstroiMapa"__ é a função responsável por transformar a grelha do mapa com todo o tipo de peças listadas no data Pecas numa lista do tipo (Peca, Coordenadas) sem o tipo "Vazio".
    Esta função fá-lo através de duas auxiliares, "auxdesconstroiMapa" e "desconstroiLinha".
-}
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa m = auxdesconstroiMapa m (0,0)

{-| __"auxdesconstroiMapa"__ invoca recursivamente a "desconstroiLinha" para ir desconstruindo uma coluna especifica da matriz, de seguida esta função volta a invocar a "desconstroiLinha" mas para trabalhar com uma coluna diferente.
-}
auxdesconstroiMapa :: Mapa -> Coordenadas -> [(Peca, Coordenadas)]
auxdesconstroiMapa [] _ = []
auxdesconstroiMapa (l:ls) (a,b)= desconstroiLinha l (a,b) ++ auxdesconstroiMapa ls (a,b+1)

{-| A função __"desconstroiLinha"__, no seguimento da "auxdesconstroiMapa", vai percorrer todas as linhas de uma determinada coluna e eliminar todas as peças do tipo "Vazio" e juntando todas as restantes com a devida posição à lista do tipo (Peca, Coordenadas).
== Exemplo:
>>>desconstroiLinha [[Vazio,Vazio,Bloco],[Porta,Vazio,Vazio],[Bloco,Bloco,Bloco]]
[(Bloco,(0,2)),(Porta,(1,0)),(Bloco,(2,0)),(Bloco,(2,1)),(Bloco,(2,2))]
-}
desconstroiLinha :: [Peca] -> Coordenadas -> [(Peca, Coordenadas)]
desconstroiLinha [] _ = []
desconstroiLinha (x:xs) (a,b)| x == Vazio = desconstroiLinha xs (a+1,b)
                             | otherwise = (x,(a,b)): desconstroiLinha xs (a+1,b)

{-| = Funções relativas à construção do mapa
-}
{-| A função __"constroiMapa"__ , contrariamente à "desconstroiMapa", e recorrendo às suas auxiliares transforma uma lista do tipo (Peca, Coordenadas) numa grelha com todas as peças, o "Vazio" inclusivé.
-}
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa l = auxConstroiMapa l 0 (indiceMatriz (jCoordenadas l))
{-| __"auxConstroiMapa"__ apenas articula as auxiliares de modo a obtermos o mapa devidamente construido em "constroiMapa"
-}
auxConstroiMapa :: [(Peca, Coordenadas)] -> Int -> Coordenadas-> Mapa
auxConstroiMapa l n (x,y) | n > y = [] 
                          | otherwise = compLinhaP (compLinha (divAndar l (0,n))  (jCoordenadas l) (0,n)) : auxConstroiMapa l (n+1) (x,y)


{-| __"compLinhaP"__ é uma auxiliar da "auxConstroiMapa" que apenas remove as coordenadas à lista de pares (Peca, Coordenadas).
== Exemplo:
>>> compLinhaP [(Bloco,(0,2)),(Porta,(1,0))]
[Bloco,Porta]
-}
compLinhaP :: [(Peca, Coordenadas)]  -> [Peca]
compLinhaP [] = []
compLinhaP ((p,(a,b)):t) =  p : compLinhaP t
{-|__"compLinha"__ compara uma lista dada pela 'divAndar' (documentada abaixo) com todas as coordenadas possíveis no mapa. 
   Nos casos em que ums peça não é declarada entre coordenadas, ou seja, quando a peça é do tipo "Vazio", a "compLinha" insere um par (Vazio,(x,y)) no respetivo local.

== Exemplo:
>>> compLinhaP [(Bloco,(0,2)),(Porta,(1,0)),(Bloco,(2,0)),(Bloco,(2,1)),(Bloco,(2,2))]
[[Vazio,Vazio,Bloco],[Porta,Vazio,Vazio],[Bloco,Bloco,Bloco]]
-}
compLinha :: [(Peca, Coordenadas)] -> [Coordenadas]  -> Coordenadas -> [(Peca, Coordenadas)]
compLinha l l1 (x,y) | x > maximum (jCoordenadasX l1) = []
compLinha [] l (x,y) = (Vazio,(x,y)): compLinha [] l (x+1,y)
compLinha ((p,(a,b)):t) l (x,y) | (x,y) `elem` l = (p,(a,b)): compLinha t l (x+1,y)
                                | otherwise = (Vazio,(x,y)): compLinha ((p,(a,b)):t) l (x+1,y)

{-|A função __"divAndar"__ procura os elementos com o mesmo y de coordenada e junta-os numa lista.
== Exemplo:
>>> divAndar [(Bloco,(0,2)),(Porta,(1,0))]
[(Bloco,(0,2))]
-}
divAndar :: [(Peca, Coordenadas)]-> Coordenadas -> [(Peca, Coordenadas)] 
divAndar [] y = []
divAndar ((p,(a,b)):t) (x,y)| y == b  = (p,(a,b)):divAndar t (x,y)
                            | otherwise = divAndar t (x,y)








