module UI where

import Lib
import Texto
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Text.Printf
import Sprite



canhaoSprite = [[0,0,0,0,0,0,0,2,2,1,2,2,0,0,0,0,0,0,0]
               ,[0,0,0,0,0,0,2,2,0,1,0,2,2,0,0,0,0,0,0]
               ,[0,0,0,0,0,2,2,0,0,1,0,0,2,2,0,0,0,0,0]
               ,[0,0,0,0,2,2,0,0,3,1,3,0,0,2,2,0,0,0,0]
               ,[0,0,0,2,2,0,0,3,3,1,3,3,0,0,2,2,0,0,0]
               ,[0,0,2,2,0,0,3,3,3,1,3,3,3,0,0,2,2,0,0]
               ,[0,2,2,0,0,0,3,3,3,1,3,3,3,0,0,0,2,2,0]
               ,[2,2,0,0,0,0,3,3,3,1,3,3,3,0,0,0,0,2,2]
               ,[0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0]]


escalaCanhao = 0.5

desenhaMundo :: Mundo -> Picture
desenhaMundo m = pictures $ [desenhaMoldura, desenhaMaxPontos, desenhaPontos, desenhaCanhao] <*> pure m
 

tratarEvento :: Event -> Mundo -> Mundo
tratarEvento (EventKey (SpecialKey KeyLeft) Down _ _) m = m {direcao = Oeste }
tratarEvento (EventKey (SpecialKey KeyRight) Down _ _) m = m {direcao = Leste }
tratarEvento (EventKey (SpecialKey KeyLeft) Up _ _) m = m {direcao = Parado }
tratarEvento (EventKey (SpecialKey KeyRight) Up _ _) m = m {direcao = Parado }
tratarEvento _ m = m


atualizaMundo :: Float -> Mundo -> Mundo
atualizaMundo _ m = m {canhao = novaPosicao}
                    where novaPosicao = moverCanhao m

moverCanhao :: Mundo -> Pos
moverCanhao (Estado (x,y) _ _ d) = (x+vel,y)
                                     where vel = case d of
                                                 Oeste -> -velocidade
                                                 Leste -> velocidade
                                                 Parado -> 0.0


rectangleThick :: Color -> Float -> Float -> Float -> Picture
rectangleThick col thick w h = pictures [color col $ rectangleSolid w h, color black $ rectangleSolid (w - thick) (h - thick)]


desenhaMaxPontos :: Mundo -> Picture 
desenhaMaxPontos m = translate (-370) (-375) $ color yellow $ texto (printf "%05d" (maxPontos m))


desenhaPontos :: Mundo -> Picture
desenhaPontos m = translate 80 (-375) $ color white $ texto (printf "%05d" (pontos m))


desenhaMoldura :: Mundo -> Picture 
desenhaMoldura _ = translate 0 (-350) $ rectangleThick magenta (tamSegmt * 2) 800 100


desenhaCanhao :: Mundo -> Picture 
desenhaCanhao m = (uncurry translate (canhao m)) $ desenhaSprite tamSegmtCanhao canhaoSprite