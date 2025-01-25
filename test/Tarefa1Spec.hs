
module Tarefa1Spec (testesTarefa1) where

import Test.HUnit
import Tarefa1
import LI12425

testesTarefa1 :: Test
testesTarefa1 =
  TestLabel "Testes Tarefa 1" $
    test
      [ teste1, teste2 ]



jogoT :: Jogo
jogoT = Jogo baseT portalT torresT mapa01 [inimigoT] lojaT

lojaT :: Loja
lojaT = [(100, Torre (0,2) 10 5 1 3 3 (Projetil Gelo (Finita 3)))]

inimigoT :: Inimigo
inimigoT = Inimigo Normal (1,1) Sul 10 0.5 10 10 [Projetil Fogo (Finita 3)] Walk 0 0 [] Nothing

baseT :: Base
baseT = Base 100 (5,4) 0

torresT :: [Torre]
torresT = [ Torre (2,2) 10 5 1 3 3 (Projetil Fogo (Finita 3)) ]


portalT :: [Portal]
portalT = [ Portal (0,0) [Onda 
                            [Inimigo Normal (0,0) Sul 10 0.5 10 10 [] Walk 0 0 [] Nothing] 
                                                                3 0 0]]


teste1 :: [Test]
teste1 = 
        [
          "T1 - Jogo Valido"      ~: True ~=? validaJogo jogoT,
          "T1 - Portais Validos"  ~: True ~=? validaPortais portalT mapa01 (5,4) torresT,
          "T1 - Inimigos Validos" ~: True ~=? validaInimigosAll portalT mapa01 torresT [inimigoT],
          "T1 - Torres Validas"   ~: True ~=? validaTorres torresT mapa01
        ]



jogoT2 :: Jogo
jogoT2 = Jogo baseT2 portalT2 torresT2 mapa01 [inimigoT2] lojaT2

lojaT2 :: Loja
lojaT2 = [(100, Torre (0,2) 10 5 1 3 3 (Projetil Gelo (Finita 3)))]

inimigoT2 :: Inimigo
inimigoT2 = Inimigo Normal (1,1) Sul 10 0.5 10 10 [] Walk 0 0 [] Nothing

baseT2 :: Base
baseT2 = Base 100 (5,4) 0

torresT2 :: [Torre]
torresT2 = [ Torre (3,2) 10 5 1 3 3 (Projetil Fogo (Finita 3)) ]


portalT2 :: [Portal]
portalT2 = [ Portal (0,0) [Onda [Inimigo Normal (0,0) Sul 10 0.5 10 10 [] Walk 0 0 [] Nothing] 3 0 0], Portal (1,2) [Onda [Inimigo Normal (1,2) Sul 10 0.5 10 10 [] Walk 0 0 [] Nothing] 3 0 0] ]


teste2 :: [Test]
teste2 = 
        [
          "T2 - Jogo Valido"      ~: False ~=? validaJogo jogoT2,
          "T2 - Portais Validos"  ~: True ~=? validaPortais portalT2 mapa01 (5,4) torresT2,
          "T2 - Inimigos Validos" ~: True ~=? validaInimigosAll portalT2 mapa01 torresT2 [inimigoT2],
          "T2 - Torres Validos"   ~: False ~=? validaTorres torresT2 mapa01
        ]