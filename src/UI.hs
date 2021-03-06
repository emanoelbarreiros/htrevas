module UI where

import Lib
import Texto
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Text.Printf
import Sprite
import Canhao
import Nave


desenhaMundo :: Mundo -> Picture
desenhaMundo m = pictures $ [desenhaMoldura, desenhaMaxPontos, desenhaPontos, desenhaCanhao, desenharNaves] <*> pure m


tratarEvento :: Event -> Mundo -> Mundo
tratarEvento (EventKey (SpecialKey KeyLeft) Down _ _) m = m {direcaoCanhao = Esquerda }
tratarEvento (EventKey (SpecialKey KeyRight) Down _ _) m = m {direcaoCanhao = Direita }
tratarEvento (EventKey (SpecialKey KeyLeft) Up _ _) m = m {direcaoCanhao = Parado }
tratarEvento (EventKey (SpecialKey KeyRight) Up _ _) m = m {direcaoCanhao = Parado }
tratarEvento (EventMotion pos) m = m { mouse = pos}
tratarEvento _ m = m


atualizaMundo :: Float -> Mundo -> Mundo
atualizaMundo _ m = m {canhao = novaPosicao, naves = novasNaves}
                    where
                        novaPosicao = moverCanhao m
                        novasNaves = atualizarNaves (mouse m) (naves m)


rectangleThick :: Color -> Float -> Float -> Float -> Picture
rectangleThick col thick w h = pictures [color col $ rectangleSolid w h, color black $ rectangleSolid (w - thick) (h - thick)]


desenhaMaxPontos :: Mundo -> Picture
desenhaMaxPontos m = translate (-370) (-375) $ color yellow $ texto (printf "%05d" (maxPontos m))


desenhaPontos :: Mundo -> Picture
desenhaPontos m = translate 80 (-375) $ color white $ texto (printf "%05d" (pontos m))


desenhaMoldura :: Mundo -> Picture
desenhaMoldura _ = translate 0 (-350) $ rectangleThick magenta (tamSegmtGrd * 2) 800 100

desenharMouse :: Mundo -> Picture
desenharMouse m = uncurry translate (mouse m) $ color white $ scale 0.2 0.2 $ text (uncurry (printf "(%.2f, %.2f)") (mouse m))