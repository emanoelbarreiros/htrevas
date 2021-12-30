module Main where

import Lib
import Graphics.Gloss
import System.Random
import UI
import Nave

mundoInicial :: StdGen -> Mundo
mundoInicial rand = Estado {
    canhao = (-38, -302.0)
  , maxPontos = 99
  , pontos = 22
  , direcaoCanhao = Parado
  , naves = [Nav (0.0, 0.0) Norte 30 rand [Noroeste, Oeste, Sudoeste, Sul, Sudeste, Leste, Norte]]
}

main :: IO ()
main = do
    stdGen <- getStdGen 
    let mundo = mundoInicial stdGen
    play (InWindow "Senhor das Trevas (tm Philips)" (tamJanela , tamJanela) (3000, 100)) black 120 mundo desenhaMundo tratarEvento atualizaMundo