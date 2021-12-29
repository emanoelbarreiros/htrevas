module Lib where


type Pos = (Float, Float)

data Direcao = Leste | Oeste | Parado deriving (Eq, Show)

data Modo = Inicio | Jogando | GameOver deriving (Eq, Show)

data Mundo = Estado {
    canhao :: Pos
  , maxPontos :: Int
  , pontos :: Int
  , direcao :: Direcao
} deriving Show


mundoInicial :: Mundo
mundoInicial = Estado {
    canhao = (0.0, -301.0)
  , maxPontos = 99
  , pontos = 22
  , direcao = Parado
}


linhas :: Num a => a
linhas = 39


limite :: (Num a, Integral a) => a
limite = linhas `div` 2


tamJanela :: Num a => a
tamJanela = tamSegmt * linhas


tamSegmt :: Num a => a
tamSegmt =  8

tamSegmtCanhao :: Num a => a
tamSegmtCanhao = 4


acaoFrames :: Num a => a
acaoFrames = 10

velocidade :: Float
velocidade = 20.0

