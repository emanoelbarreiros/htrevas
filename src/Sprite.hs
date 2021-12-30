module Sprite where

import Graphics.Gloss
import Lib


cor :: [Color]
cor = [black, white, green, greyN 0.5, magenta, blue, red]


desenhaSegmento :: Bool -> Float -> (Float, Picture) -> Int -> (Float, Picture)
desenhaSegmento usarPaleta t (off, p) 0 = (off + t, p)
desenhaSegmento usarPaleta t (off, p) c = (off + t, pictures [p, novaPic])
                                  where 
                                      pic = translate off 0 $ rectangleSolid t t
                                      novaPic = if usarPaleta then color (cor !! c) pic else pic


desenhaLinha :: Bool -> Float -> [Int] -> Picture
desenhaLinha dc t xs = snd $ foldl (desenhaSegmento dc t) (0, blank) xs


desenhaSprite :: Bool -> Float -> [[Int]] -> Picture
desenhaSprite _ t [] = blank
desenhaSprite usarPaleta t bs = pictures $ zipWith (translate 0) ([fromIntegral x * t | x <- [0..(length bs)]]) (reverse $ map (desenhaLinha usarPaleta t) bs) 