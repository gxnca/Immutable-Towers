module Niveis where

import LI12425

-- | Loja constante para todos os níveis
lojaConst :: Loja
lojaConst  = [ (100, Torre (0,0) 2 2 2 5 0 (Projetil Fogo (Finita 3))),
               (100, Torre (0,0) 3 2 2 5 0 (Projetil Gelo (Finita 2))),          -- trẽs tipos básicos de torre...
               (100, Torre (0,0) 2 2 2 5 0 (Projetil Resina (Finita 4)))
             ]

-- | Nível 1
nivel1 :: Jogo
nivel1 = Jogo {
  baseJogo=Base 100 (17,1) 300,
  portaisJogo=[
    Portal (0,3) [
      Onda [
        Inimigo Normal (0,3) Este 10 0.5 20 18 [] Walk 1 0 [] Nothing,
        Inimigo Normal (0,3) Este 10 0.5 20 18 [] Walk 2 0 [] Nothing
      ] 3 3 0,

      Onda [
        Inimigo Normal (0,3) Este 10 0.5 20 18 [] Walk 1 0 [] Nothing,
        Inimigo Normal (0,3) Este 10 0.5 20 18 [] Walk 2 0 [] Nothing,
        Inimigo Normal (0,3) Este 10 0.5 20 18 [] Walk 3 0 [] Nothing,
        Inimigo Normal (0,3) Este 10 0.5 20 18 [] Walk 4 0 [] Nothing
      ] 3 3 15,

      Onda [
        Inimigo Normal (0,3) Este 10 0.5 20 18 [] Walk 1 0 [] Nothing,
        Inimigo Normal (0,3) Este 10 0.5 20 18 [] Walk 2 0 [] Nothing,
        Inimigo Normal (0,3) Este 10 0.5 20 18 [] Walk 3 0 [] Nothing,
        Inimigo Normal (0,3) Este 10 0.5 20 18 [] Walk 4 0 [] Nothing
      ] 3 3 37,

      Onda [
        Inimigo Tanque (0,3) Este 30 0.4 35 28 [] Walk 1 0 [] Nothing
      ] 1 1 52
    ]

    ],
  torresJogo=[],
  mapaJogo=mapa,
  inimigosJogo=[],
  lojaJogo=lojaConst
}
  where mapa = [ [r, r, r, r, r, r, a, a, a, a, r, r, r, r ,r ,r, r, r],
                 [r, r, r, r, r, r, r, a, a, r, r, r, t, t ,t ,t, t, t],
                 [r, r, r, r, r, r, r, r, r, r, r, r, t, r ,r ,r, r, r],
                 [t, t, t, t, r, r, r, t, t, r, r, r, t, r ,r ,r, r, r],
                 [r, r, r, t, r, r, t, t, t, t, r, r, t, r ,r ,r, r, r],
                 [r, r, r, t, r, r, t, r, r, t, r, r, t, r ,r ,r, r, r],
                 [r, a, r, t, t, t, t, r, r, t, t, t, t, r ,r ,t, r, r],
                 [r, a, r, r, r, r, r, r, r, r, r, r, r, r ,t ,t, t, r],
                 [r, a, a, r, r, r, r, a, a, a, r, r, r, r ,r ,t, t, r],
                 [r, r, r, r, r, r, r, r, a, a, a, r, r, r ,r ,r, r, r]
               ]
               where
                  t = Terra
                  r = Relva
                  a = Agua


-- | Nível 2
nivel2 :: Jogo
nivel2 = Jogo {
  baseJogo=Base 100 (17,1) 300,
  portaisJogo=[
    Portal (2,0) [
      Onda [
        Inimigo Normal (2,0) Este 10 0.5 20 18 [] Walk 1 0 [] Nothing,
        Inimigo Normal (2,0) Este 10 0.5 20 18 [] Walk 2 0 [] Nothing,
        Inimigo Normal (2,0) Este 10 0.5 20 18 [] Walk 3 0 [] Nothing,
        Inimigo Normal (2,0) Este 10 0.5 20 18 [] Walk 4 0 [] Nothing
        ] 3 3 0,

        Onda [
        Inimigo Normal (2,0) Este 10 0.5 20 18 [] Walk 1 0 [] Nothing,
        Inimigo Tanque (2,0) Este 30 0.4 35 30 [] Walk 2 0 [] Nothing,
        Inimigo Tanque (2,0) Este 30 0.4 35 30 [] Walk 3 0 [] Nothing,
        Inimigo Normal (2,0) Este 10 0.5 20 18 [] Walk 4 0 [] Nothing
        ] 3 3 45
    ],

    Portal (1,9) [
      Onda [
        Inimigo Normal (1,9) Norte 10 0.5 20 18 [] Walk 1 0 [] Nothing,
        Inimigo Normal (1,9) Norte 10 0.5 20 18 [] Walk 2 0 [] Nothing,
        Inimigo Normal (1,9) Norte 10 0.5 20 18 [] Walk 3 0 [] Nothing,
        Inimigo Normal (1,9) Norte 10 0.5 20 18 [] Walk 4 0 [] Nothing
        ] 3 3 20,

        Onda [
        Inimigo Normal (1,9) Este 10 0.5 20 18 [] Walk 1 0 [] Nothing,
        Inimigo Tanque (1,9) Este 30 0.4 35 30 [] Walk 2 0 [] Nothing
        ] 3 3 45

    ]
    ],
  torresJogo=[],
  mapaJogo=mapa,
  inimigosJogo=[],
  lojaJogo=lojaConst
}
  where mapa = [ [r, r, t, t, r, r, a, a, a, a, r, r, r, r ,r ,r, r, r],
                 [r, r, r, t, r, r, r, a, a, r, r, r, t, t ,t ,t, t, t],
                 [r, t, r, t, r, r, r, r, r, r, r, r, t, r ,r ,r, r, r],
                 [r, t, r, t, t, t, t, t, t, t, r, r, t, r ,r ,r, r, r],
                 [r, r, r, r, r, r, r, r, r, t, r, r, t, r ,r ,r, r, r],
                 [r, r, r, r, r, r, r, a, r, t, t, t, t, r ,r ,a, a, r],
                 [r, t, t, t, t, t, r, r, r, t, r, r, r, r ,a ,a, a, r],
                 [r, t, r, r, r, t, t, t, t, t, r, r, r, a ,a ,a, a, r],
                 [r, t, r, r, r, r, r, r, r, r, r, r, r, a ,a ,a, r, r],
                 [r, t, r, r, r, r, r, r, r, r, r, r, r, r ,r ,r, r, r]
               ]
               where
                  t = Terra
                  r = Relva
                  a = Agua


-- | Nível 3
nivel3 :: Jogo
nivel3 = Jogo {
  baseJogo=Base 100 (17,4) 400,
  portaisJogo=[
    Portal (0,2) [
      Onda [
        Inimigo Rapido (0,2) Este 7 0.8 15 10 [] Walk 1 0 [] Nothing,
        Inimigo Rapido (0,2) Este 7 0.8 15 10 [] Walk 2 0 [] Nothing,
        Inimigo Rapido (0,2) Este 7 0.8 15 10 [] Walk 3 0 [] Nothing,
        Inimigo Rapido (0,2) Este 7 0.8 15 10 [] Walk 4 0 [] Nothing
        ] 1 3 3,

        Onda [
        Inimigo Normal (0,2) Norte 10 0.5 20 18 [] Walk 1 0 [] Nothing,
        Inimigo Normal (0,2) Norte 10 0.5 20 18 [] Walk 2 0 [] Nothing
        ] 3 3 13
    ],

    Portal (2,9) [
      Onda [
        Inimigo Normal (2,9) Norte 10 0.5 20 18 [] Walk 1 0 [] Nothing,
        Inimigo Normal (2,9) Norte 10 0.5 20 18 [] Walk 2 0 [] Nothing,
        Inimigo Normal (2,9) Norte 10 0.5 20 18 [] Walk 3 0 [] Nothing,
        Inimigo Normal (2,9) Norte 10 0.5 20 18 [] Walk 4 0 [] Nothing
        ] 3 3 22,

        Onda [
        Inimigo Rapido (2,9) Este 7 0.8 15 10 [] Walk 1 0 [] Nothing,
        Inimigo Rapido (2,9) Este 7 0.8 15 10 [] Walk 2 0 [] Nothing
        ] 1 3 34
    ], 

        Portal (11,9) [
      Onda [
        Inimigo Rapido (11,9) Este 7 0.8 15 10 [] Walk 1 0 [] Nothing,
        Inimigo Rapido (11,9) Este 7 0.8 15 10 [] Walk 2 0 [] Nothing,
        Inimigo Rapido (11,9) Este 7 0.8 15 10 [] Walk 3 0 [] Nothing,
        Inimigo Rapido (11,9) Este 7 0.8 15 10 [] Walk 4 0 [] Nothing
        ] 1 4 0,

        Onda [
        Inimigo Normal (11,9) Norte 10 0.5 20 18 [] Walk 1 0 [] Nothing,
        Inimigo Normal (11,9) Norte 10 0.5 20 18 [] Walk 2 0 [] Nothing
        ] 3 3 5
    ]
    
    ],
  torresJogo=[],
  mapaJogo=mapa,
  inimigosJogo=[],
  lojaJogo=lojaConst
}
  where mapa = [ [r, r, r, r, r, r, a, a, a, a, r, r, r, r ,r ,r, r, r],
                 [r, r, r, r, r, r, r, a, a, r, r, r, r, r ,r ,r, r, r],
                 [t, t, t, t, t, t, r, r, r, t, t, t, t, r ,r ,r, r, r],
                 [r, r, t, r, r, t, t, t, t, t, r, r, t, t ,t ,t, t, t],
                 [r, r, t, r, r, t, t, t, t, t, r, r, r, t ,t ,t, t, t],
                 [r, r, t, t, t, t, r, a, r, t, t, t, r, r ,r ,a, a, r],
                 [r, r, t, r, r, r, r, r, r, r, r, t, r, r ,a ,a, a, r],
                 [r, r, t, r, r, a, a, a, a, r, r, t, r, a ,a ,a, a, r],
                 [r, r, t, r, r, a, a, a, a, r, r, t, r, a ,a ,a, r, r],
                 [r, r, t, r, r, r, r, r, r, r, r, t, r, r ,r ,r, r, r]
               ]
               where
                  t = Terra
                  r = Relva
                  a = Agua


-- | Nível 4
nivel4 :: Jogo
nivel4 = Jogo {
  baseJogo=Base 100 (17,1) 470,
  portaisJogo=[
    Portal (5,0) [
      Onda [
        Inimigo Rapido (5,0) Este 7 0.8 15 10 [] Walk 1 0 [] Nothing,
        Inimigo Rapido (5,0) Este 7 0.8 15 10 [] Walk 2 0 [] Nothing,
        Inimigo Rapido (5,0) Este 7 0.8 15 10 [] Walk 3 0 [] Nothing,
        Inimigo Rapido (5,0) Este 7 0.8 15 10 [] Walk 4 0 [] Nothing
        ] 1 3 0,

      Onda [
        Inimigo Rapido (5,0) Este 7 0.8 15 10 [] Walk 1 0 [] Nothing,
        Inimigo Rapido (5,0) Este 7 0.8 15 10 [] Walk 2 0 [] Nothing,
        Inimigo Rapido (5,0) Este 7 0.8 15 10 [] Walk 3 0 [] Nothing,
        Inimigo Rapido (5,0) Este 7 0.8 15 10 [] Walk 4 0 [] Nothing
        ] 1 3 21,
      
      Onda [
        Inimigo Tanque (5,0) Norte 30 0.4 35 30 [] Walk 1 0 [] Nothing,
        Inimigo Tanque (5,0) Norte 30 0.4 35 30 [] Walk 2 0 [] Nothing
        ] 2 2 28,

      Onda [
        Inimigo Normal (5,0) Norte 10 0.5 20 18 [] Walk 1 0 [] Nothing,
        Inimigo Normal (5,0) Norte 10 0.5 20 18 [] Walk 2 0 [] Nothing,
        Inimigo Tanque (5,0) Norte 30 0.4 35 30 [] Walk 3 0 [] Nothing
        ] 3 3 50
    ],

    Portal (6,9) [
      Onda [
        Inimigo Tanque (6,9) Norte 30 0.4 35 30 [] Walk 1 0 [] Nothing,
        Inimigo Tanque (6,9) Norte 30 0.4 35 30 [] Walk 2 0 [] Nothing
        ] 2 2 0,

      Onda [
        Inimigo Normal (6,9) Norte 10 0.5 20 18 [] Walk 1 0 [] Nothing,
        Inimigo Normal (6,9) Norte 10 0.5 20 18 [] Walk 2 0 [] Nothing,
        Inimigo Tanque (6,9) Norte 30 0.4 35 30 [] Walk 3 0 [] Nothing
        ] 3 3 50
    ], 

        Portal (0,4) [
      Onda [
        Inimigo Normal (0,4) Norte 10 0.5 20 18 [] Walk 1 0 [] Nothing,
        Inimigo Normal (0,4) Norte 10 0.5 20 18 [] Walk 2 0 [] Nothing,
        Inimigo Normal (0,4) Norte 10 0.5 20 18 [] Walk 3 0 [] Nothing,
        Inimigo Normal (0,4) Norte 10 0.5 20 18 [] Walk 4 0 [] Nothing
        ] 3 3 21,

      Onda [
        Inimigo Normal (0,4) Norte 10 0.5 20 18 [] Walk 1 0 [] Nothing,
        Inimigo Normal (0,4) Norte 10 0.5 20 18 [] Walk 2 0 [] Nothing,
        Inimigo Tanque (0,4) Norte 30 0.4 35 30 [] Walk 3 0 [] Nothing
        ] 3 3 50
    ]
    
    ],
  torresJogo=[],
  mapaJogo=mapa,
  inimigosJogo=[],
  lojaJogo=lojaConst
}
  where mapa = [ [r, r, r, r, r, t, t, r, r, r, r, r, r, r ,r ,r, r, r],
                 [r, r, r, r, r, r, t, r, r, r, r, r, r, r ,r ,t, t, t],
                 [r, r, r, r, r, r, t, r, r, r, r, r, r, r ,r ,t, r, r],
                 [r, r, r, r, t, t, t, t, t, r, r, r, t, t ,t ,t, r, r],
                 [t, t, t, t, t, r, r, r, t, r, r, r, t, r ,r ,r, r, r],
                 [r, r, r, r, t, r, r, r, t, t, t, t, t, t ,t ,t, t, t],
                 [r, t, r, r, t, t, t, t, t, r, r, r, t, t ,t ,t, r, r],
                 [r, t, t, r, r, r, t, r, r, r, r, r, r, r ,r ,r, r, r],
                 [r, t, t, t, r, r, t, r, r, r, r, r, r, a ,r ,a, a, r],
                 [r, r, r, r, r, r, t, r, r, r, r, r, a, a ,a ,a, r, r]
               ]
               where
                  t = Terra
                  r = Relva
                  a = Agua

  
-- | Nível 5
nivel5 :: Jogo
nivel5 = Jogo {
  baseJogo=Base 100 (17,7) 500,
  portaisJogo=[
    Portal (3,0) [

      Onda [
          Inimigo Normal (3,0) Este 10 0.5 20 18 [] Walk 1 0 [] Nothing,
          Inimigo Normal (3,0) Este 10 0.5 20 18 [] Walk 2 0 [] Nothing,
          Inimigo Normal (3,0) Este 10 0.5 20 18 [] Walk 3 0 [] Nothing,
          Inimigo Normal (3,0) Este 10 0.5 20 18 [] Walk 4 0 [] Nothing
        ] 3 3 0,

      Onda [
          Inimigo Tanque (3,0) Este 30 0.4 35 30 [] Walk 1 0 [] Nothing
        ] 1 3 22,
      
      Onda [
          Inimigo Normal (3,0) Este 10 0.5 20 18 [] Walk 1 0 [] Nothing,
          Inimigo Normal (3,0) Este 10 0.5 20 18 [] Walk 2 0 [] Nothing,
          Inimigo Normal (3,0) Este 10 0.5 20 18 [] Walk 3 0 [] Nothing,
          Inimigo Normal (3,0) Este 10 0.5 20 18 [] Walk 4 0 [] Nothing
        ] 3 3 45

    ],

    Portal (0,6) [
      Onda [
          Inimigo Rapido (0,6) Este 7 0.8 15 10 [] Walk 1 0 [] Nothing,
          Inimigo Rapido (0,6) Este 7 0.8 15 10 [] Walk 2 0 [] Nothing,
          Inimigo Rapido (0,6) Este 7 0.8 15 10 [] Walk 3 0 [] Nothing,
          Inimigo Rapido (0,6) Este 7 0.8 15 10 [] Walk 4 0 [] Nothing,
          Inimigo Rapido (0,6) Este 7 0.8 15 10 [] Walk 5 0 [] Nothing,
          Inimigo Rapido (0,6) Este 7 0.8 15 10 [] Walk 6 0 [] Nothing
        ] 1 3 22,

      Onda [
          Inimigo Tanque (0,6) Este 30 0.4 35 30 [] Walk 1 0 [] Nothing
        ] 1 3 36,

      Onda [
          Inimigo Normal (0,6) Este 10 0.5 20 18 [] Walk 1 0 [] Nothing,
          Inimigo Normal (0,6) Este 10 0.5 20 18 [] Walk 2 0 [] Nothing,
          Inimigo Normal (0,6) Este 10 0.5 20 18 [] Walk 3 0 [] Nothing,
          Inimigo Normal (0,6) Este 10 0.5 20 18 [] Walk 4 0 [] Nothing
        ] 3 3 45
    ], 

        Portal (8,9) [
      Onda [
          Inimigo Normal (8,9) Este 10 0.5 20 18 [] Walk 1 0 [] Nothing,
          Inimigo Normal (8,9) Este 10 0.5 20 18 [] Walk 2 0 [] Nothing,
          Inimigo Normal (8,9) Este 10 0.5 20 18 [] Walk 3 0 [] Nothing
        ] 3 3 0,

      Onda [
          Inimigo Rapido (8,9) Este 7 0.8 15 10 [] Walk 1 0 [] Nothing,
          Inimigo Rapido (8,9) Este 7 0.8 15 10 [] Walk 2 0 [] Nothing
        ] 1 3 36
    ]
    
    ],
  torresJogo=[],
  mapaJogo=mapa,
  inimigosJogo=[],
  lojaJogo=lojaConst
}
  where mapa = [ [r, r, r, t, t, r, r, r, r, r, r, r, r, r ,a ,r, r, r],
                 [r, r, r, r, t, r, r, r, r, r, r, r, r, r ,a ,r, r, r],
                 [r, r, r, r, t, r, r, t, t, t, r, r, r, r ,a ,a, r, r],
                 [a, a, r, r, t, t, t, t, t, t, t, t, t, t ,r ,a, a, r],
                 [r, a, a, r, r, r, r, t, t, t, r, r, r, t ,r ,r, a, r],
                 [r, r, a, r, r, r, r, r, t, r, r, r, r, t ,t ,t, t, t],
                 [t, t, t, t, t, t, t, t, t, r, t, t, t, r ,r ,a, a, t],
                 [r, r, a, r, r, r, r, r, r, t, t, a, t, r ,r ,a, t, t],
                 [r, a, a, a, a, a, a, r, r, t, a, a, t, t ,t ,t, t, r],
                 [r, r, r, r, r, r, a, r, t, t, a, a, a, a ,a ,a, r, r]
               ]
               where
                  t = Terra
                  r = Relva
                  a = Agua