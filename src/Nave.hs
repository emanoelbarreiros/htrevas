module Nave where

import Sprite
import Graphics.Gloss
import Lib
import Canhao (velocidade)
import System.Random


type Ordem = Direcao

limiteArena :: Float
limiteArena = 450

limiteArenaInf :: Float
limiteArenaInf = -150

velocidadeNave :: Float
velocidadeNave = 3.0

framesDirecao :: Int
framesDirecao = 25

velocidadeInclinada :: Float
velocidadeInclinada = velocidadeNave * 0.707106


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
        (_, melhorDirecao) = foldl menorDist (100000.0, Norte) (zip distancias possibs)
        novaPos = melhorDirecao <+> pos


menorDist :: (Float, Direcao) -> (Float, Direcao) -> (Float, Direcao)
menorDist t1@(minDist, melhorDir) t2@(dist, dir) = if dist < minDist then t2 else t1

atualizarNaves :: Pos -> [Nave] -> [Nave]
atualizarNaves mouse = map (atualizarNave mouse)


distancia :: Pos -> Pos -> Float
distancia (x1,y1) (x2,y2) = sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2