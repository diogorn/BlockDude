{- |
Module      : Tarefa1_2021li1g083
Description : Validação de um potencial mapa
Copyright   : Diogo Neto <a98197@alunos.uminho.pt>;
            : Diogo Afonso <a101919@alunos.uminho.pt>;

__@Tarefa 1 do projeto de LI1 em 2021/22@__

Este módulo, referente à tarefa 1, contém definições Haskell para avaliar se um potencial mapa do jogo __Block Dude__ é jogável de acordo com cinco regras.
    1.  Apenas uma peça por posição
    2.  Apenas uma porta no jogo
    3.  Não existem caixas flutuantes
    4.  O mapa tem  que ter pelo menos um espaço vazio
    5.  Deve exisir chão ao longo do mapa
-}
module Tarefa1_2021li1g083 where
import LI12122
import Data.Ix


{- | A função __"validaPotencialMapa"__ verifica que as regras do jogo são cumpridas utilizando funções auxiliares individuais para cada regra
-}
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa l = contaPortas l == 1 && checkDuplos(jCoordenadas l) && checkBase l l && checkVazios (jCoordenadas l) (criaMatriz l) && chao l (primeiroChao l)

{-| = Regra 1
-}
{- | __"checkDuplos"__ verifica, recursivamente, que numa lista de apenas coordenadas não existem pares ordenados repetidos de modo a satisfazer a regra número 1.
-}
checkDuplos :: [Coordenadas] -> Bool
checkDuplos [] = True
checkDuplos ((x,y):t) | (x,y) `elem` t = False
                      | otherwise = checkDuplos t
{- | __"jCoordenadas"__ é auxiliar da função "checkDuplos" e é responsável por obter apenas as coordenadas das peças descartando o seu tipo 
-}
jCoordenadas :: [(Peca, Coordenadas)] -> [Coordenadas]
jCoordenadas [] = []
jCoordenadas ((p,(x,y)):t) = (x,y):jCoordenadas t

{-| = Regra 2
-}

{- | A função __"contaPortas"__ assegura que a regra número 2 é cumprida contando o número de portas que existem na lista do tipo (peca,coordenadas) atribuida na "validaPotencialMapa"
-}
contaPortas :: [(Peca, Coordenadas)] -> Int
contaPortas [] = 0
contaPortas ((p,c):t) | p == Porta = 1 + contaPortas t
                      | otherwise = contaPortas t

{-| = Regra 3
-}

{- | __"checkBase"__ irá localizar as peças do tipo /caixa/ no mapa e a sua auxiliar "checkBloco" irá verificar se a base dessa peça é outra /caixa/ ou um /bloco/ evitando assim a existência de caixas fluantes como manda a regra número 3   -}
checkBase :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> Bool
checkBase [] l = True
checkBase ((p,(x,y)):t) l | p == Caixa && checkBloco (x,y+1) l == False = False
                          | otherwise = checkBase t l
{- | __"checkBloco"__ verifica se uma /caixa/ tem uma base válida
-}
checkBloco :: Coordenadas -> [(Peca, Coordenadas)]-> Bool
checkBloco (xc,yc) l | (Bloco,(xc,yc)) `elem` l  || (Caixa,(xc,yc)) `elem` l = True
                     | otherwise = False


{- | __"jCoordenadasX"__ é auxiliar da função "indiceMatriz" e é responsável por seperar os pares ordenados da lista das coordenadas nume lista com apenas os elementos X
-}
jCoordenadasX :: [Coordenadas] -> [Int]
jCoordenadasX [] = []
jCoordenadasX ((x,y):t) = x : jCoordenadasX t

{- | __"jCoordenadasY"__ é auxiliar da função "indiceMatriz" e é responsável por seperar os pares ordenados da lista das coordenadas nume lista com apenas os elementos Y
-}
jCoordenadasY :: [Coordenadas] -> [Int]
jCoordenadasY [] = []
jCoordenadasY ((x,y):t) = y : jCoordenadasY t

{- | __"indiceMatriz"__ obtém um par ordenado que corresponde ao indice da matriz criada pela "criaMatriz".
     Esta função identifica qual o maior elemento das listas criadas pelas funções "jCoordenadasX" e "jCoordenadasY" e retorna o indice da matriz
-}
indiceMatriz :: [Coordenadas] -> Coordenadas
indiceMatriz l = (maximum (jCoordenadasX l), maximum (jCoordenadasY l))

{- | Como já referido na defnição do checkVazios a função __"criaMatriz__" irá criar uma matriz de indice m*n e criar todas posições possiveis nesse mapa desde (0,0) até (m,n)   
-}
criaMatriz :: [(Peca, Coordenadas)] -> [Coordenadas]
criaMatriz l = range((0,0),indiceMatriz (jCoordenadas l))

{-| = Regra 4
-}
{- | __"checkVazios"__ irá verificar se existem espaços vazios como pede a regra número 4.
     Esta função recebe a lista das coordenadas das peças no mapa dado (lista obtida pela "jCoordenadas") e uma matriz de indice m*n com todas as coordenadas existentes (inclusive as não listadas).
     A matriz criada pela "criaMatriz" serve para compararmos as coordenadas das peças do mapa com a mesma. Comparando a lista das peças do mapa inserido com a lista de todas as coordenadas possiveis iremos concluir que se sobrarem pares ordenadas na lista criada pela "criaMatriz" então existem espaços vazios.

-}
checkVazios :: [Coordenadas] -> [Coordenadas] -> Bool
checkVazios l [] = False
checkVazios l ((m,n):ms) | (m,n) `notElem` l = True
                         | otherwise = checkVazios l ms


{- | __"primeiroChao"__ é responsável por detectar o primeiro bloco que serve de chão
-}
primeiroChao :: [(Peca, Coordenadas)] -> (Peca, Coordenadas)
primeiroChao ((p,(a,b)):(p2,(a2,b2)):t)|  a == a2 = primeiroChao ((p2,(a2,b2)):t)
                                       | otherwise =  (p,(a,b))

{-| = Regra 5
-}

{-| __"chao"__ verifica se há chão ao longo de todo o mapa, obedecendo à  regra 5. Esta função considera que se os Blocos tiverem apenas uma ligação por esquina, terão que ter uma caixa a liga-los, como no exemplo do Q&A
-}
chao:: [(Peca, Coordenadas)] -> (Peca, Coordenadas)->Bool 
chao  l  p | last l == p = True
chao _ (p,(a,b)) | p /= Bloco && p/= Caixa = False
chao l (p,(a,b)) | (Bloco,(a,b+1)) `elem` l = chao (delete (p,(a,b)) l) (Bloco,(a,b+1))
                 | (Bloco,(a+1,b)) `elem` l = chao (delete (p,(a,b)) l) (Bloco,(a+1,b))
                 | (Bloco,(a,b-1)) `elem` l = chao (delete (p,(a,b)) l) (Bloco,(a,b-1))
                 | (Caixa,(a,b-1)) `elem` l = chao (delete (p,(a,b)) l) (Caixa,(a,b-1))
                 | otherwise = False
{- | __"delete"__ é uma auxiliar das funções do chao que elimina da lista a peça anteriormente comparada para a função não entrar em loop
-}
delete :: Eq a => a -> [a] -> [a]
delete deleted xs = [ x | x <- xs, x /= deleted ]


{- | __"chaoalt"__ é uma função alternativa à "chao" Esta função considera que os Blocos podem se ligar apenas pela sua esquina. Inativa de momento
-}
chaoalt:: [(Peca, Coordenadas)] -> (Peca, Coordenadas)-> Bool 
chaoalt  l  p | last l == p = True
chaoalt _ (p,(a,b)) | p /= Bloco = False
chaoalt l (p,(a,b)) | (Bloco,(a,b+1)) `elem` l = chaoalt (delete (p,(a,b)) l) (Bloco,(a,b+1))
                    | (Bloco,(a+1,b+1)) `elem` l = chaoalt (delete (p,(a,b)) l) (Bloco,(a+1,b+1))
                    | (Bloco,(a+1,b)) `elem` l = chaoalt (delete (p,(a,b)) l) (Bloco,(a+1,b))
                    | (Bloco,(a+1,b-1)) `elem` l = chaoalt (delete (p,(a,b)) l) (Bloco,(a+1,b-1))
                    | (Bloco,(a,b-1)) `elem` l = chaoalt (delete (p,(a,b)) l) (Bloco,(a,b-1))
                    | otherwise = False
