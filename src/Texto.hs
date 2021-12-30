module Texto where

import Graphics.Gloss
import Lib
import Sprite

char0 :: [[Int]]
char0 = [[0,1,1,1,1,1,0]
        ,[1,1,0,0,0,1,1]
        ,[1,1,0,0,0,1,1]
        ,[1,1,0,0,0,1,1]
        ,[1,1,0,0,0,1,1]
        ,[1,1,0,0,0,1,1]
        ,[0,1,1,1,1,1,0]]

char1 = [[0,0,0,1,1,0,0]
        ,[0,0,1,1,1,0,0]
        ,[0,0,0,1,1,0,0]
        ,[0,0,0,1,1,0,0]
        ,[0,0,0,1,1,0,0]
        ,[0,0,0,1,1,0,0]
        ,[0,0,1,1,1,1,0]]

char2 = [[0,1,1,1,1,1,0]
        ,[1,1,0,0,0,1,1]
        ,[0,0,0,0,1,1,0]
        ,[0,0,0,1,1,0,0]
        ,[0,0,1,1,0,0,0]
        ,[0,1,1,0,0,0,0]
        ,[1,1,1,1,1,1,1]]

char3 = [[0,1,1,1,1,1,0]
        ,[1,1,0,0,0,1,1]
        ,[0,0,0,0,0,1,1]
        ,[0,0,0,1,1,1,0]
        ,[0,0,0,0,0,1,1]
        ,[1,1,0,0,0,1,1]
        ,[0,1,1,1,1,1,0]]

char4 = [[1,1,0,0,1,1,0]
        ,[1,1,0,0,1,1,0]
        ,[1,1,0,0,1,1,0]
        ,[1,1,1,1,1,1,1]
        ,[0,0,0,0,1,1,0]
        ,[0,0,0,0,1,1,0]
        ,[0,0,0,0,1,1,0]]

char5 = [[1,1,1,1,1,1,1]
        ,[1,1,0,0,0,0,0]
        ,[1,1,0,0,0,0,0]
        ,[0,1,1,1,1,1,0]
        ,[0,0,0,0,0,1,1]
        ,[1,0,0,0,0,1,1]
        ,[0,1,1,1,1,1,0]]

char6 = [[0,1,1,1,1,1,0]
        ,[1,1,0,0,0,1,1]
        ,[1,1,0,0,0,0,0]
        ,[1,1,1,1,1,1,0]
        ,[1,1,0,0,0,1,1]
        ,[1,1,0,0,0,1,1]
        ,[0,1,1,1,1,1,0]]

char7 = [[1,1,1,1,1,1,1]
        ,[0,0,0,0,0,1,1]
        ,[0,0,0,0,1,1,0]
        ,[0,0,0,1,1,0,0]
        ,[0,0,1,1,0,0,0]
        ,[0,1,1,0,0,0,0]
        ,[1,1,0,0,0,0,0]]

char8 = [[0,1,1,1,1,1,0]
        ,[1,1,0,0,0,1,1]
        ,[1,1,0,0,0,1,1]
        ,[0,1,1,1,1,1,0]
        ,[1,1,0,0,0,1,1]
        ,[1,1,0,0,0,1,1]
        ,[0,1,1,1,1,1,0]]

char9 = [[0,1,1,1,1,1,0]
        ,[1,1,0,0,0,1,1]
        ,[1,1,0,0,0,1,1]
        ,[0,1,1,1,1,1,1]
        ,[0,0,0,0,0,1,1]
        ,[1,1,0,0,0,1,1]
        ,[0,1,1,1,1,1,0]]


caractere :: Char -> Picture
caractere c = case c of
    '0' -> desenhaSprite False tamSegmtGrd char0
    '1' -> desenhaSprite False tamSegmtGrd char1
    '2' -> desenhaSprite False tamSegmtGrd char2
    '3' -> desenhaSprite False tamSegmtGrd char3
    '4' -> desenhaSprite False tamSegmtGrd char4
    '5' -> desenhaSprite False tamSegmtGrd char5
    '6' -> desenhaSprite False tamSegmtGrd char6
    '7' -> desenhaSprite False tamSegmtGrd char7
    '8' -> desenhaSprite False tamSegmtGrd char8
    '9' -> desenhaSprite False tamSegmtGrd char9
    _   -> blank


texto :: String -> Picture 
texto s = snd $ foldl desenhaTexto (0, blank) (map caractere s)


desenhaTexto :: (Float, Picture) -> Picture -> (Float, Picture)
desenhaTexto (off, c) p = (off + 7*tamSegmtGrd + (tamSegmtGrd / 2), pictures [c, shiftDireita off p])


shiftDireita :: Float -> Picture -> Picture
shiftDireita d = translate d 0