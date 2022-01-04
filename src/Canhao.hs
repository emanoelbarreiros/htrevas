module Canhao where

import Lib
import Graphics.Gloss
import Sprite


canhaoSprite :: Sprite
canhaoSprite = [[0,0,0,0,0,0,0,2,2,1,2,2,0,0,0,0,0,0,0]
               ,[0,0,0,0,0,0,2,2,0,1,0,2,2,0,0,0,0,0,0]
               ,[0,0,0,0,0,2,2,0,0,1,0,0,2,2,0,0,0,0,0]
               ,[0,0,0,0,2,2,0,0,3,1,3,0,0,2,2,0,0,0,0]
               ,[0,0,0,2,2,0,0,3,3,1,3,3,0,0,2,2,0,0,0]
               ,[0,0,2,2,0,0,3,3,3,1,3,3,3,0,0,2,2,0,0]
               ,[0,2,2,0,0,0,3,3,3,1,3,3,3,0,0,0,2,2,0]
               ,[2,2,0,0,0,0,3,3,3,1,3,3,3,0,0,0,0,2,2]
               ,[0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0]]

velocidade :: Float
velocidade = 20.0


intervaloCanhao :: (Float, Float)
intervaloCanhao = (-398.0, 326.0)


desenhaCanhao :: Mundo -> Picture
desenhaCanhao m = uncurry translate (canhao m) $ desenhaSprite True tamSegmtPeq canhaoSprite

moverCanhao :: Mundo -> Pos
moverCanhao (Estado (x,y) _ _ d _ _) = (limite intervaloCanhao (x+vel),y)
                                         where vel = case d of
                                                     Esquerda -> -velocidade
                                                     Direita -> velocidade
                                                     _ -> 0.0