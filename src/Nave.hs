module Nave where

import Sprite
import Graphics.Gloss
import Lib
import Canhao (velocidade)
import System.Random

velocidadeNave :: Float
velocidadeNave = 4.0

framesDirecao :: Int
framesDirecao = 20

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
Parado   <+> tup   = tup


possiveisDirecoes :: Direcao -> [Direcao]
possiveisDirecoes d = case d of
                      Norte -> [Norte, Noroeste, Nordeste]
                      Sul -> [Sul, Sudoeste, Sudeste]
                      Leste -> [Sudeste, Leste, Nordeste]
                      Oeste -> [Oeste, Sudoeste, Noroeste]
                      Sudeste -> [Sudeste, Sul, Leste]
                      Nordeste -> [Nordeste, Leste, Norte]
                      Noroeste -> [Noroeste, Norte, Oeste]
                      Sudoeste -> [Sudoeste, Oeste, Sul]
                      Parado -> [Parado]


naveSprite :: Sprite
naveSprite = [[1,1,1,1,1,1,1]
             ,[0,1,1,1,1,1,0]]


desenharNave :: Nave -> Picture
desenharNave (Nav (x,y) _ _ _ _) = translate x y $ desenhaSprite False tamSegmtMed naveSprite


desenharNaves :: Mundo -> Picture
desenharNaves m = color yellow $ pictures $ map desenharNave (naves m)


atualizarNave :: Nave -> Nave
atualizarNave (Nav pos dir fra rand ordens)
    | fra == 0 = Nav (novaDir <+> pos) novaDir framesDirecao novoRand novasOrdens
    | otherwise = Nav (dir <+> pos) dir (fra - 1) rand ordens
    where
        (novaDir, novoRand, novasOrdens) = if not (null ordens) then (head ordens, rand, tail ordens) else novaDirecao dir rand |> []



novaDirecao :: Direcao -> StdGen -> (Direcao, StdGen)
novaDirecao dir rand = (novaDir, novoRand)
                    where
                        direcoes = possiveisDirecoes dir
                        (indice, novoRand) = randomR (0 :: Int, length direcoes - 1) rand
                        novaDir = direcoes !! indice

atualizarNaves :: [Nave] -> [Nave]
atualizarNaves = map atualizarNave