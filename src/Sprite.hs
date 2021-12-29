module Sprite where

import Graphics.Gloss
import Lib


cor :: [Color]
cor = [black, white, green, greyN 0.5, magenta, blue, red]


desenhaSegmento :: Float -> (Float, Picture) -> Int -> (Float, Picture)
desenhaSegmento t (off, p) 0 = (off + t, p)
desenhaSegmento t (off, p) c = (off + t, pictures [p, translate off 0 $ color (cor !! c) $ rectangleSolid t t])


desenhaLinha :: Float -> [Int] -> Picture
desenhaLinha t xs = snd $ foldl (desenhaSegmento t) (0, blank) xs


desenhaSprite :: Float -> [[Int]] -> Picture
desenhaSprite t [] = blank
desenhaSprite t bs = pictures $ zipWith (translate 0) ([fromIntegral x * t | x <- [0..(length bs)]]) (reverse $ map (desenhaLinha t) bs) 