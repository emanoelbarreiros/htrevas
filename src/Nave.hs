module Nave where

import Sprite
import Graphics.Gloss
import Lib
import Canhao (velocidade)
import System.Random
import Data.CircularList
import Data.Maybe
import Data.Semigroup (Min(Min))


type Ordem = Direcao

limiteArena :: Float
limiteArena = 450

limiteArenaInf :: Float
limiteArenaInf = -150

velocidadeNave :: Float
velocidadeNave = 4.0

framesDirecao :: Int
framesDirecao = 20

velocidadeInclinada :: Float
velocidadeInclinada = velocidadeNave * 0.707106

direcoes :: CList Direcao
direcoes = fromList [Leste, Nordeste, Norte, Noroeste, Oeste, Sudoeste, Sul, Sudeste]


(<+>) :: Direcao -> (Float, Float) -> (Float, Float)
Norte    <+> (x,y) = (x, y + velocidadeNave)
Sul      <+> (x,y) = (x, y - velocidadeNave)
Leste    <+> (x,y) = (x + velocidadeNave, y)
Oeste    <+> (x,y) = (x - velocidadeNave, y)
Sudeste  <+> (x,y) = (x + velocidadeInclinada, y - velocidadeInclinada)
Nordeste <+> (x,y) = (x + velocidadeInclinada, y + velocidadeInclinada)
Noroeste <+> (x,y) = (x - velocidadeInclinada, y + velocidadeInclinada)
Sudoeste <+> (x,y) = (x - velocidadeInclinada, y - velocidadeInclinada)


possiveisDirecoes :: Direcao -> [Direcao]
possiveisDirecoes d = case d of
                      Norte    -> [Norte, Noroeste, Nordeste]
                      Sul      -> [Sul, Sudoeste, Sudeste]
                      Leste    -> [Sudeste, Leste, Nordeste]
                      Oeste    -> [Oeste, Sudoeste, Noroeste]
                      Sudeste  -> [Sudeste, Sul, Leste]
                      Nordeste -> [Nordeste, Leste, Norte]
                      Noroeste -> [Noroeste, Norte, Oeste]
                      Sudoeste -> [Sudoeste, Oeste, Sul]


naveSprite :: Sprite
naveSprite = [[1,1,1,1,1,1,1]
             ,[0,1,1,1,1,1,0]]


desenharNave :: Nave -> Picture
desenharNave (Nav (x,y) _ _ _ _) = translate x y $ desenhaSprite False tamSegmtMed naveSprite


desenharNaves :: Mundo -> Picture
desenharNaves m = color yellow $ pictures $ map desenharNave (naves m)


atualizarNave :: Pos -> Nave -> Nave
atualizarNave mouse (Nav pos dir fra rand ordens)
    | fra == 0 = Nav novaPos melhorDirecao framesDirecao rand []
    | otherwise = Nav novaPos dir (fra - 1) rand ordens
    where
        possibs = possiveisDirecoes dir
        posicoes = map (<+> pos) possibs
        distancias = map (distancia mouse) posicoes
        (_, melhorDirecao) = foldl (\(minDist, melhorDir) (dist, dir) -> if dist < minDist then (dist, dir) else (minDist, melhorDir)) (100000.0, Norte) (zip distancias possibs)
        novaPos = melhorDirecao <+> pos


foraArena :: Pos -> Bool
foraArena p = or $ [\(x,_) -> x > limiteArena, \(x,_) -> x < (-limiteArena), \(_,y) -> y > limiteArena, \(_,y) -> y < limiteArenaInf] <*> pure p


novaDirecao :: CList Direcao -> StdGen -> (CList Direcao, StdGen)
novaDirecao dir rand = (novaDir, novoRand)
                    where
                        (rotacao, novoRand) = randomR (0, 2) rand :: (Int, StdGen)
                        novaDir = [esquerda dir, dir, direita dir] !! rotacao -- melhorar isso aqui depois, mas tô sem caso agora...


atualizarNaves :: Pos -> [Nave] -> [Nave]
atualizarNaves mouse = map (atualizarNave mouse)


direita :: CList Direcao -> CList Direcao
direita = rotR


esquerda :: CList Direcao -> CList Direcao
esquerda = rotL


distancia :: Pos -> Pos -> Float
distancia (x1,y1) (x2,y2) = sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2