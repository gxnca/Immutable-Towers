module Tarefa3Spec (testesTarefa3) where

import Test.HUnit
import LI12425
import Tarefa3

testesTarefa3 :: Test
testesTarefa3 =
  TestLabel "Testes Tarefa 3" $
    test
      [ teste1,
        teste2,
        teste3
      ]

mapa01 :: Mapa
mapa01 = [ [t, t, r, a, a, a],
           [r, t, r, a, r, r],
           [r, t, r, a, r, t],
           [r, t, r, a, r, t],
           [r, t, t, t, t, t],
           [a, a, a, a, r, r]
         ]
         where
            t = Terra
            r = Relva
            a = Agua

-- Estados do jogo
jogoT :: Jogo
jogoT = Jogo baseT portalT torresT mapa01 [inimigoT] lojaT

jogoTA :: Jogo
jogoTA = Jogo baseT portalTA torresTA mapa01 [inimigoTA'] lojaT
  where
    inimigoTA' = Inimigo Normal (1,1.5) Norte 9 0.5 10 10 [Projetil Fogo (Finita 2)] Walk 1 0 
                 [(1.0,2.0),(1.0,3.0),(1.0,4.0),(2.0,4.0),(3.0,4.0),(4.0,4.0),(5.0,4.0)]
                 (Just ((1.0,1.0),(1.0,2.0),0.5))

jogoT2 :: Jogo
jogoT2 = Jogo baseT portalT2 torresT mapa01 [inimigoT2] lojaT

jogoT2A :: Jogo
jogoT2A = Jogo baseT portalT2A torresTA mapa01 [inimigoTA2'] lojaT
  where
    inimigoTA2' = Inimigo Normal (1.6,1.0) Oeste 7 0.4 12 12 [Projetil Fogo (Finita 3)] Walk 1 0 
                  [(1.0,1.0),(1.0,2.0),(1.0,3.0),(1.0,4.0),(2.0,4.0),(3.0,4.0),(4.0,4.0),(5.0,4.0)]
                  (Just ((2.0,1.0),(1.0,1.0),0.39999998))

jogoT3 :: Jogo
jogoT3 = Jogo baseT portalT3 torresT mapa01 [inimigoT3] lojaT

jogoT3A :: Jogo
jogoT3A = Jogo baseT portalT3A torresTA mapa01 [inimigoT3A', inimigoNovo2'] lojaT
  where
    inimigoT3A' = inimigoT3A
    inimigoNovo2' = Inimigo Normal (0.5,0.0) Este 10 0.5 10 10 [] Walk 1 0 
                    [(1.0,0.0),(1.0,1.0),(1.0,2.0),(1.0,3.0),(1.0,4.0),(2.0,4.0),(3.0,4.0),(4.0,4.0),(5.0,4.0)]
                    (Just ((0.0,0.0),(1.0,0.0),0.5))

-- Loja
lojaT :: Loja
lojaT = [(100, Torre (0,2) 10 5 1 3 3 (Projetil Gelo (Finita 3)))]

-- Inimigos
inimigoT :: Inimigo
inimigoT = Inimigo Normal (1,1) Sul 10 0.5 10 10 [Projetil Fogo (Finita 3)] Walk 1 0 [] Nothing

inimigoT2 :: Inimigo
inimigoT2 = Inimigo Normal (2,1) Oeste 8 0.4 12 12 [Projetil Fogo (Finita 4)] Walk 1 0 [] Nothing

inimigoT3 :: Inimigo
inimigoT3 = Inimigo Normal (3,2) Oeste 9 0.7 15 15 [Projetil Gelo (Finita 5)] Walk 1 0 [] Nothing

inimigoT3A :: Inimigo
inimigoT3A = Inimigo Normal (3.0,2.0) Oeste 9 0.7 15 15 [Projetil Gelo (Finita 4)] Frozen 1 0 [] Nothing


-- Base
baseT :: Base
baseT = Base 100 (5,4) 0

-- Torres
torresT :: [Torre]
torresT = [ Torre (2,2) 10 5 1 3 3 (Projetil Fogo (Finita 3)) ]

torresTA :: [Torre]
torresTA = [ Torre (2,2) 10 5 1 3 2 (Projetil Fogo (Finita 3)) ]

-- Portais
portalT :: [Portal]
portalT = [ Portal (0,0) [Onda [Inimigo Normal (0,0) Sul 10 0.5 10 10 [] Walk 1 0 [] Nothing] 3 3 0] ]

portalTA :: [Portal]
portalTA = [ Portal (0,0) [Onda [Inimigo Normal (0,0) Sul 10 0.5 10 10 [] Walk 1 0 [] Nothing] 3 2 0] ]

portalT2 :: [Portal]
portalT2 = [ Portal (0,0) [Onda [Inimigo Rapido (0,0) Sul 10 0.5 10 10 [] Walk 1 0 [] Nothing] 3 3 5] ]

portalT2A :: [Portal]
portalT2A = [ Portal (0,0) [Onda [Inimigo Rapido (0,0) Sul 10 0.5 10 10 [] Walk 1 0 [] Nothing] 3 3 4] ]

portalT3 :: [Portal]
portalT3 = [ Portal (0,0) [Onda [Inimigo Normal (0,0) Sul 10 0.5 10 10 [] Walk 1 0 [] Nothing] 3 0 0] ]

portalT3A :: [Portal]
portalT3A = [ Portal (0,0) [Onda [] 3 3 0] ]

-- Testes
teste1 :: Test
teste1 = "T3 - AtualizaJogo" ~: jogoTA ~=? atualizaJogo 1 jogoT

teste2 :: Test
teste2 = "T3 - AtualizaJogo 2" ~: jogoT2A ~=? atualizaJogo 1 jogoT2

teste3 :: Test
teste3 = "T3 - AtualizaJogo 3" ~: jogoT3A ~=? atualizaJogo 1 jogoT3