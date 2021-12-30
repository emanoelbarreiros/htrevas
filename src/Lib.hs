module Lib where

import System.Random

-- (x,y)
type Pos = (Float, Float)

data Direcao = Norte | Sul | Leste | Oeste | Nordeste | Sudoeste | Sudeste | Noroeste | Parado deriving Eq

data Modo = Inicio | Jogando | GameOver deriving Eq

-- (posicao, direcao, frames para renovar direcao)
data Nave = Nav {
    posicao :: Pos
  , direcaoNave :: Direcao
  , renovarDir :: Int
  , rand :: StdGen
  , ordens :: [Direcao]
} deriving Eq


tamJanela :: Int
tamJanela = 900


data Mundo = Estado {
    canhao :: Pos
  , maxPontos :: Int
  , pontos :: Int
  , direcaoCanhao :: Direcao
  , naves :: [Nave]
}


limite :: (Float, Float) -> Float -> Float
limite (li,ls) v
  | v < li = li
  | v > ls = ls
  | otherwise = v

(|>) :: (a,b) -> c -> (a,b,c)
(a,b) |> c = (a,b,c)