module Desenhar where

import Graphics.Gloss
import ImmutableTowers
import LI12425

import Data.Maybe (fromJust)



-- | A função 'desenha' é a função principal de Desenho. Faz o desenho total do jogo
desenha :: ImmutableTowers -> IO Picture
desenha it@ImmutableTowers{estadoRato=rato, currentLevel=level, currentMenu=menu} = case menu of
    InGame -> return $ Pictures [mapDrawPic it (mapaJogo level), atualizaHoover it rato, drawLevel it level, drawUI it]
    _      -> drawMenu it


data NomesImagens = ImagemTorreGelo | ImagemEcraVitoria1Estrela 


-- Utilities:

-- | A função 'spriteToGloss' torna o ponto de referência de um sprite, o canto inferior esquerdo
spriteTranslate :: Picture -> Picture
spriteTranslate = Translate (tileSize/2) (tileSize/2)

-- | A função 'centerSprite' é usada para obter a posição central relativa a uma tile de uma construção.
centerSprite :: Posicao -> Posicao
centerSprite (x,y) = (x*tileSize+buildSize,y*tileSize+2*buildSize)

-- | A função 'mapToScreen' é usada para converter uma posição relativa ao mapa, numa posição relativa ao ecrá.
mapToScreen :: Posicao -> Picture -> Picture
mapToScreen (x,y) = Translate (x-mapWidth/2) (mapHeight/2-y)


{- | A função 'getSprites' é utilizada para carregar todos os sprites necessários para inicializar o jogo,
armazenando-os no estado de jogo (ImmutableTowers)
-}
getSprites :: ImmutableTowers -> IO ImmutableTowers
getSprites it = do -- Terreno e Mapa
                   let basePath = "assets/"
                   terraS <- loadBMP "assets/terreno/terra.bmp"
                   relvaS <- loadBMP "assets/terreno/relva.bmp"
                   aguaS  <- loadBMP "assets/terreno/agua.bmp"

                   -- GUI
                   hooverSR      <- loadBMP "assets/ui/hooverR.bmp"
                   hooverSB      <- loadBMP "assets/ui/hooverB.bmp"
                   fogoUI        <- loadBMP "assets/ui/fogoUI.bmp"
                   geloUI        <- loadBMP "assets/ui/geloUI.bmp"
                   resinaUI      <- loadBMP "assets/ui/resinaUI.bmp"
                   buildUI       <- loadBMP "assets/ui/buildUI.bmp"
                   fogoSelecUI   <- loadBMP "assets/ui/fogoSelecUI.bmp"
                   geloSelecUI   <- loadBMP "assets/ui/geloSelecUI.bmp"
                   resinaSelecUI <- loadBMP "assets/ui/resinaSelecUI.bmp"
                   returnBUI     <- loadBMP "assets/ui/returnBUI.bmp"

                   healthUI      <- loadBMP "assets/ui/healthUI.bmp"
                   creditosUI    <- loadBMP "assets/ui/creditsUI.bmp"
                   waveUI        <- loadBMP "assets/ui/waves.bmp"

                   -- Menu
                   playButton         <- loadBMP "assets/ui/menu/icon/playButton.bmp"
                   playBSelec         <- loadBMP "assets/ui/menu/icon/playButtonSelected.bmp"
                   playMini           <- loadBMP "assets/ui/menu/icon/playMini.bmp"
                   playMiniSelected   <- loadBMP "assets/ui/menu/icon/playMiniSelected.bmp"
                   tutorialButton     <- loadBMP "assets/ui/menu/icon/tutorialButton.bmp"
                   tutorialBSelec     <- loadBMP "assets/ui/menu/icon/tutorialButtonSelected.bmp"
                   exitButton         <- loadBMP "assets/ui/menu/icon/exitButton.bmp"
                   exitButtonSelected <- loadBMP "assets/ui/menu/icon/exitButtonSelected.bmp"
                   bgMenu             <- loadBMP "assets/ui/menu/background/bgMenu.bmp"
                   bgLS               <- loadBMP "assets/ui/menu/background/bgLS.bmp"
                   gamewinBG1          <- loadBMP "assets/ui/menu/background/bgWin1.bmp"
                   gamewinBG2          <- loadBMP "assets/ui/menu/background/bgWin2.bmp"
                   gamewinBG3          <- loadBMP "assets/ui/menu/background/bgWin3.bmp"
                   gameoverBG         <- loadBMP "assets/ui/menu/background/gameoverBG.bmp"
                   keysHelp           <- loadBMP "assets/ui/menu/keysHelp.bmp"
                   restartButton      <- loadBMP "assets/ui/menu/icon/restartButton.bmp"
                   restartBSelec      <- loadBMP "assets/ui/menu/icon/restartButtonSelected.bmp"
                   levelMenuButton    <- loadBMP "assets/ui/menu/icon/levelMenuButton.bmp"
                   levelMenuBSelec    <- loadBMP "assets/ui/menu/icon/levelMenuButtonSelected.bmp"
                   level1             <- loadBMP "assets/ui/menu/icon/level1.bmp"
                   level1S            <- loadBMP "assets/ui/menu/icon/level1S.bmp"
                   level2             <- loadBMP "assets/ui/menu/icon/level2.bmp"
                   level2S            <- loadBMP "assets/ui/menu/icon/level2S.bmp"
                   level3             <- loadBMP "assets/ui/menu/icon/level3.bmp"
                   level3S            <- loadBMP "assets/ui/menu/icon/level3S.bmp"
                   level4             <- loadBMP "assets/ui/menu/icon/level4.bmp"
                   level4S            <- loadBMP "assets/ui/menu/icon/level4S.bmp"
                   level5             <- loadBMP "assets/ui/menu/icon/level5.bmp"
                   level5S            <- loadBMP "assets/ui/menu/icon/level5S.bmp"
                   backBtn            <- loadBMP "assets/ui/menu/icon/backBtn.bmp"
                   backBtnS           <- loadBMP "assets/ui/menu/icon/backBtnS.bmp"
                   lockLevel          <- loadBMP "assets/ui/menu/icon/lockLevel.bmp"
                   lockLevelSelec     <- loadBMP "assets/ui/menu/icon/lockLevelSelected.bmp"

                   -- Tutorial imgs:
                   tutorial1 <- loadBMP "assets/ui/menu/tutorial/Tutorial1.bmp"
                   tutorial2 <- loadBMP "assets/ui/menu/tutorial/Tutorial2.bmp"
                   tutorial3 <- loadBMP "assets/ui/menu/tutorial/Tutorial3.bmp"
                   tutorial4 <- loadBMP "assets/ui/menu/tutorial/Tutorial4.bmp"

                   -- Numeros
                   white0   <- loadBMP "assets/ui/numbers/white0.bmp"
                   white1   <- loadBMP "assets/ui/numbers/white1.bmp"
                   white2   <- loadBMP "assets/ui/numbers/white2.bmp"
                   white3   <- loadBMP "assets/ui/numbers/white3.bmp"
                   white4   <- loadBMP "assets/ui/numbers/white4.bmp"
                   white5   <- loadBMP "assets/ui/numbers/white5.bmp"
                   white6   <- loadBMP "assets/ui/numbers/white6.bmp"
                   white7   <- loadBMP "assets/ui/numbers/white7.bmp"
                   white8   <- loadBMP "assets/ui/numbers/white8.bmp"
                   white9   <- loadBMP "assets/ui/numbers/white9.bmp"

                   creditos0   <- loadBMP "assets/ui/numbers/creditos0.bmp"
                   creditos1   <- loadBMP "assets/ui/numbers/creditos1.bmp"
                   creditos2   <- loadBMP "assets/ui/numbers/creditos2.bmp"
                   creditos3   <- loadBMP "assets/ui/numbers/creditos3.bmp"
                   creditos4   <- loadBMP "assets/ui/numbers/creditos4.bmp"
                   creditos5   <- loadBMP "assets/ui/numbers/creditos5.bmp"
                   creditos6   <- loadBMP "assets/ui/numbers/creditos6.bmp"
                   creditos7   <- loadBMP "assets/ui/numbers/creditos7.bmp"
                   creditos8   <- loadBMP "assets/ui/numbers/creditos8.bmp"
                   creditos9   <- loadBMP "assets/ui/numbers/creditos9.bmp"

                   baseS <- loadBMP "assets/base.bmp"

                   portalS <- loadBMP "assets/portal.bmp"

                   torreGelo <- loadBMP "assets/torreGeloTeste.bmp"
                   torreFogo <- loadBMP "assets/torreFogoTeste.bmp"
                   torreResina <- loadBMP "assets/torreResinaTeste.bmp"
                   torreGeloPrev <- loadBMP "assets/torreGeloPrev.bmp"
                   torreFogoPrev <- loadBMP "assets/torreFogoPrev.bmp"
                   torreResinaPrev <- loadBMP "assets/torreResinaPrev.bmp"

                   fire1 <- loadBMP "assets/fire1.bmp"
                   fire2 <- loadBMP "assets/fire2.bmp"
                   fire3 <- loadBMP "assets/fire3.bmp"
                   fire4 <- loadBMP "assets/fire4.bmp"

                   -- Inimigos
                   inimigoFrozen <- loadBMP "assets/inimigos/frozen.bmp"

                   inimigo1_1  <- loadBMP "assets/inimigos/inimigo1/inimigo1_1.bmp"
                   inimigo1_2  <- loadBMP "assets/inimigos/inimigo1/inimigo1_2.bmp"
                   inimigo1_3  <- loadBMP "assets/inimigos/inimigo1/inimigo1_3.bmp"
                   inimigo1_4  <- loadBMP "assets/inimigos/inimigo1/inimigo1_4.bmp"
                   inimigo1_5  <- loadBMP "assets/inimigos/inimigo1/inimigo1_4.bmp"
                   inimigo1_6  <- loadBMP "assets/inimigos/inimigo1/inimigo1_6.bmp"
                   inimigo1_7  <- loadBMP "assets/inimigos/inimigo1/inimigo1_7.bmp"
                   inimigo1_8  <- loadBMP "assets/inimigos/inimigo1/inimigo1_8.bmp"
                   inimigo1_9  <- loadBMP "assets/inimigos/inimigo1/inimigo1_9.bmp"
                   inimigo1_10 <- loadBMP "assets/inimigos/inimigo1/inimigo1_10.bmp"
                   inimigo1_11 <- loadBMP "assets/inimigos/inimigo1/inimigo1_11.bmp"
                   inimigo1_12 <- loadBMP "assets/inimigos/inimigo1/inimigo1_12.bmp"
                   inimigo1_13 <- loadBMP "assets/inimigos/inimigo1/inimigo1_13.bmp"
                   inimigo1_14 <- loadBMP "assets/inimigos/inimigo1/inimigo1_14.bmp"
                   inimigo1_15 <- loadBMP "assets/inimigos/inimigo1/inimigo1_15.bmp"
                   inimigo1_16 <- loadBMP "assets/inimigos/inimigo1/inimigo1_16.bmp"
                   inimigo1_17 <- loadBMP "assets/inimigos/inimigo1/inimigo1_17.bmp"
                   inimigo1_18 <- loadBMP "assets/inimigos/inimigo1/inimigo1_18.bmp"
                   inimigo1_19 <- loadBMP "assets/inimigos/inimigo1/inimigo1_19.bmp"
                   inimigo1_20 <- loadBMP "assets/inimigos/inimigo1/inimigo1_20.bmp"

                   inimigo2_1  <- loadBMP "assets/inimigos/inimigo2/inimigo2_1.bmp"
                   inimigo2_2  <- loadBMP "assets/inimigos/inimigo2/inimigo2_2.bmp"
                   inimigo2_3  <- loadBMP "assets/inimigos/inimigo2/inimigo2_3.bmp"
                   inimigo2_4  <- loadBMP "assets/inimigos/inimigo2/inimigo2_4.bmp"
                   inimigo2_5  <- loadBMP "assets/inimigos/inimigo2/inimigo2_4.bmp"
                   inimigo2_6  <- loadBMP "assets/inimigos/inimigo2/inimigo2_6.bmp"
                   inimigo2_7  <- loadBMP "assets/inimigos/inimigo2/inimigo2_7.bmp"
                   inimigo2_8  <- loadBMP "assets/inimigos/inimigo2/inimigo2_8.bmp"
                   inimigo2_9  <- loadBMP "assets/inimigos/inimigo2/inimigo2_9.bmp"
                   inimigo2_10 <- loadBMP "assets/inimigos/inimigo2/inimigo2_10.bmp"
                   inimigo2_11 <- loadBMP "assets/inimigos/inimigo2/inimigo2_11.bmp"
                   inimigo2_12 <- loadBMP "assets/inimigos/inimigo2/inimigo2_12.bmp"
                   inimigo2_13 <- loadBMP "assets/inimigos/inimigo2/inimigo2_13.bmp"
                   inimigo2_14 <- loadBMP "assets/inimigos/inimigo2/inimigo2_14.bmp"
                   inimigo2_15 <- loadBMP "assets/inimigos/inimigo2/inimigo2_15.bmp"
                   inimigo2_16 <- loadBMP "assets/inimigos/inimigo2/inimigo2_16.bmp"
                   inimigo2_17 <- loadBMP "assets/inimigos/inimigo2/inimigo2_17.bmp"
                   inimigo2_18 <- loadBMP "assets/inimigos/inimigo2/inimigo2_18.bmp"
                   inimigo2_19 <- loadBMP "assets/inimigos/inimigo2/inimigo2_19.bmp"
                   inimigo2_20 <- loadBMP "assets/inimigos/inimigo2/inimigo2_20.bmp"

                   inimigo3_1  <- loadBMP "assets/inimigos/inimigo3/inimigo3_1.bmp"
                   inimigo3_2  <- loadBMP "assets/inimigos/inimigo3/inimigo3_2.bmp"
                   inimigo3_3  <- loadBMP "assets/inimigos/inimigo3/inimigo3_3.bmp"
                   inimigo3_4  <- loadBMP "assets/inimigos/inimigo3/inimigo3_4.bmp"
                   inimigo3_5  <- loadBMP "assets/inimigos/inimigo3/inimigo3_4.bmp"
                   inimigo3_6  <- loadBMP "assets/inimigos/inimigo3/inimigo3_6.bmp"
                   inimigo3_7  <- loadBMP "assets/inimigos/inimigo3/inimigo3_7.bmp"
                   inimigo3_8  <- loadBMP "assets/inimigos/inimigo3/inimigo3_8.bmp"
                   inimigo3_9  <- loadBMP "assets/inimigos/inimigo3/inimigo3_9.bmp"
                   inimigo3_10 <- loadBMP "assets/inimigos/inimigo3/inimigo3_10.bmp"
                   inimigo3_11 <- loadBMP "assets/inimigos/inimigo3/inimigo3_11.bmp"
                   inimigo3_12 <- loadBMP "assets/inimigos/inimigo3/inimigo3_12.bmp"
                   inimigo3_13 <- loadBMP "assets/inimigos/inimigo3/inimigo3_13.bmp"
                   inimigo3_14 <- loadBMP "assets/inimigos/inimigo3/inimigo3_14.bmp"
                   inimigo3_15 <- loadBMP "assets/inimigos/inimigo3/inimigo3_15.bmp"
                   inimigo3_16 <- loadBMP "assets/inimigos/inimigo3/inimigo3_16.bmp"
                   inimigo3_17 <- loadBMP "assets/inimigos/inimigo3/inimigo3_17.bmp"
                   inimigo3_18 <- loadBMP "assets/inimigos/inimigo3/inimigo3_18.bmp"
                   inimigo3_19 <- loadBMP "assets/inimigos/inimigo3/inimigo3_19.bmp"
                   inimigo3_20 <- loadBMP "assets/inimigos/inimigo3/inimigo3_20.bmp"

                   --Projeteis:

                        -- sprites de fogo
                   fogoS_1  <- loadBMP "assets/projeteis/fogo/fogo_1.bmp"
                   fogoS_2  <- loadBMP "assets/projeteis/fogo/fogo_2.bmp"
                   fogoS_3  <- loadBMP "assets/projeteis/fogo/fogo_3.bmp"
                   fogoS_4  <- loadBMP "assets/projeteis/fogo/fogo_4.bmp"
                   fogoS_5  <- loadBMP "assets/projeteis/fogo/fogo_5.bmp"

                       -- sprites de gelo
                   gelo_1   <- loadBMP "assets/projeteis/gelo/gelo_1.bmp"
                   gelo_2   <- loadBMP "assets/projeteis/gelo/gelo_2.bmp"
                   gelo_3   <- loadBMP "assets/projeteis/gelo/gelo_3.bmp"
                   gelo_4   <- loadBMP "assets/projeteis/gelo/gelo_4.bmp"
                   gelo_5   <- loadBMP "assets/projeteis/gelo/gelo_5.bmp"

                       -- sprites de resina
                   resina_1 <- loadBMP "assets/projeteis/resina/resina_1.bmp"
                   resina_2 <- loadBMP "assets/projeteis/resina/resina_2.bmp"
                   resina_3 <- loadBMP "assets/projeteis/resina/resina_3.bmp"
                   resina_4 <- loadBMP "assets/projeteis/resina/resina_4.bmp"
                   resina_5 <- loadBMP "assets/projeteis/resina/resina_5.bmp"

                   -- Torres:

                   return it {
                    sprites= [
                        ("terra", terraS),
                        ("relva", relvaS),
                        ("agua", aguaS),

                        ("hooverR", hooverSR),
                        ("hooverB", hooverSB),
                        ("FogoUI", fogoUI),
                        ("GeloUI", geloUI),
                        ("ResinaUI", resinaUI),
                        ("buildUI", buildUI),
                        ("FogoSelecUI", fogoSelecUI),
                        ("GeloSelecUI", geloSelecUI),
                        ("ResinaSelecUI", resinaSelecUI),
                        ("returnBUI", returnBUI),

                        ("healthUI", healthUI),
                        ("creditosUI", creditosUI),
                        ("waveUI", waveUI),

                        ("playButton", playButton),
                        ("playButtonSelected", playBSelec),
                        ("playMini", playMini),
                        ("playMiniSelected", playMiniSelected),
                        ("tutorialButton", tutorialButton),
                        ("tutorialButtonSelected", tutorialBSelec),
                        ("exitButton", exitButton),
                        ("exitButtonSelected", exitButtonSelected),
                        ("bgMenu", bgMenu),
                        ("bgLS", bgLS),
                        ("gamewinBG1", gamewinBG1),
                        ("gamewinBG2", gamewinBG2),
                        ("gamewinBG3", gamewinBG3),
                        ("gameoverBG", gameoverBG),
                        ("keysHelp", keysHelp),
                        ("restartButton", restartButton),
                        ("restartButtonSelected", restartBSelec),
                        ("levelMenuButton", levelMenuButton),
                        ("levelMenuButtonSelected", levelMenuBSelec),
                        ("level1",level1),
                        ("level1S",level1S),
                        ("level2",level2),
                        ("level2S",level2S),
                        ("level3",level3),
                        ("level3S",level3S),
                        ("level4",level4),
                        ("level4S",level4S),
                        ("level5",level5),
                        ("level5S",level5S),
                        ("backBtn",backBtn),
                        ("backBtnS", backBtnS),
                        ("lockLevel", lockLevel),
                        ("lockLevelSelec", lockLevelSelec),

                        ("tutorial1", tutorial1),
                        ("tutorial2", tutorial2),
                        ("tutorial3", tutorial3),
                        ("tutorial4", tutorial4),

                        ("white0", white0),
                        ("white1", white1),
                        ("white2", white2),
                        ("white3", white3),
                        ("white4", white4),
                        ("white5", white5),
                        ("white6", white6),
                        ("white7", white7),
                        ("white8", white8),
                        ("white9", white9),

                        ("creditos0", creditos0),
                        ("creditos1", creditos1),
                        ("creditos2", creditos2),
                        ("creditos3", creditos3),
                        ("creditos4", creditos4),
                        ("creditos5", creditos5),
                        ("creditos6", creditos6),
                        ("creditos7", creditos7),
                        ("creditos8", creditos8),
                        ("creditos9", creditos9),


                        ("base", baseS),

                        ("portal", portalS),

                        ("torreGelo", torreGelo),
                        ("torreFogo", torreFogo),
                        ("torreResina", torreResina),
                        ("torreGeloPrev", torreGeloPrev),
                        ("torreFogoPrev", torreFogoPrev),
                        ("torreResinaPrev", torreResinaPrev),

                        ("fire1", fire1),
                        ("fire2", fire2),
                        ("fire3", fire3),
                        ("fire4", fire4),

                        ("frozen", inimigoFrozen),

                        ("inimigo1_1", inimigo1_1),
                        ("inimigo1_2", inimigo1_2),
                        ("inimigo1_3", inimigo1_3),
                        ("inimigo1_4", inimigo1_4),
                        ("inimigo1_5", inimigo1_5),
                        ("inimigo1_6", inimigo1_6),
                        ("inimigo1_7", inimigo1_7),
                        ("inimigo1_8", inimigo1_8),
                        ("inimigo1_9", inimigo1_9),
                        ("inimigo1_10", inimigo1_10),
                        ("inimigo1_11", inimigo1_11),
                        ("inimigo1_12", inimigo1_12),
                        ("inimigo1_13", inimigo1_13),
                        ("inimigo1_14", inimigo1_14),
                        ("inimigo1_15", inimigo1_15),
                        ("inimigo1_16", inimigo1_16),
                        ("inimigo1_17", inimigo1_17),
                        ("inimigo1_18", inimigo1_18),
                        ("inimigo1_19", inimigo1_19),
                        ("inimigo1_20", inimigo1_20),

                        ("inimigo2_1", inimigo2_1),
                        ("inimigo2_2", inimigo2_2),
                        ("inimigo2_3", inimigo2_3),
                        ("inimigo2_4", inimigo2_4),
                        ("inimigo2_5", inimigo2_5),
                        ("inimigo2_6", inimigo2_6),
                        ("inimigo2_7", inimigo2_7),
                        ("inimigo2_8", inimigo2_8),
                        ("inimigo2_9", inimigo2_9),
                        ("inimigo2_10", inimigo2_10),
                        ("inimigo2_11", inimigo2_11),
                        ("inimigo2_12", inimigo2_12),
                        ("inimigo2_13", inimigo2_13),
                        ("inimigo2_14", inimigo2_14),
                        ("inimigo2_15", inimigo2_15),
                        ("inimigo2_16", inimigo2_16),
                        ("inimigo2_17", inimigo2_17),
                        ("inimigo2_18", inimigo2_18),
                        ("inimigo2_19", inimigo2_19),
                        ("inimigo2_20", inimigo2_20),

                        ("inimigo3_1", inimigo3_1),
                        ("inimigo3_2", inimigo3_2),
                        ("inimigo3_3", inimigo3_3),
                        ("inimigo3_4", inimigo3_4),
                        ("inimigo3_5", inimigo3_5),
                        ("inimigo3_6", inimigo3_6),
                        ("inimigo3_7", inimigo3_7),
                        ("inimigo3_8", inimigo3_8),
                        ("inimigo3_9", inimigo3_9),
                        ("inimigo3_10", inimigo3_10),
                        ("inimigo3_11", inimigo3_11),
                        ("inimigo3_12", inimigo3_12),
                        ("inimigo3_13", inimigo3_13),
                        ("inimigo3_14", inimigo3_14),
                        ("inimigo3_15", inimigo3_15),
                        ("inimigo3_16", inimigo3_16),
                        ("inimigo3_17", inimigo3_17),
                        ("inimigo3_18", inimigo3_18),
                        ("inimigo3_19", inimigo3_19),
                        ("inimigo3_20", inimigo3_20),


                        ("fogo1", fogoS_1),
                        ("fogo2", fogoS_2),
                        ("fogo3", fogoS_3),
                        ("fogo4", fogoS_4),
                        ("fogo5", fogoS_5),

                        ("gelo1", gelo_1),
                        ("gelo2", gelo_2),
                        ("gelo3", gelo_3),
                        ("gelo4", gelo_4),
                        ("gelo5", gelo_5),

                        ("resina1", resina_1),
                        ("resina2", resina_2),
                        ("resina3", resina_3),
                        ("resina4", resina_4),
                        ("resina5", resina_5)

                    ]
                        }


-- | Função auxiliar para obter o sprite correto baseado no tipo e frame
getSpriteProjetil :: ImmutableTowers -> TipoProjetil -> Int -> Picture
getSpriteProjetil it tipo frame =
    let spriteKey = case tipo of
            Fogo   -> "fogo" ++ show frame
            Gelo   -> "gelo" ++ show frame
            Resina -> "resina" ++ show frame
    in fromJust (lookup spriteKey (sprites it))


-- Dimensões Gerais
tileSize :: Float
tileSize  = 96       -- ^ Dimensão de cada tile de terreno
buildSize :: Float
buildSize = 32

linhas :: Float
linhas    = 10        -- ^ Número de linhas do mapa
colunas :: Float
colunas   = 18        -- ^ Número de colunas do mapa

-- Calculo de dimensão do Mapa
mapWidth :: Float
mapWidth   = tileSize * colunas
mapHeight :: Float
mapHeight = tileSize * linhas




-- Grafismo de uma Tile de terreno

-- | Desenho básico de tile
tile :: Picture
tile = Polygon [(0,0),(tileSize,0),(tileSize,tileSize),(0,tileSize)]

-- | Sprite Tile de relva
relva :: ImmutableTowers -> Picture
relva ImmutableTowers {sprites=sprites} = spriteTranslate $ fromJust (lookup "relva" sprites)

-- | Sprite Tile de terra
terra :: ImmutableTowers -> Picture
terra ImmutableTowers {sprites=sprites} = spriteTranslate $ fromJust (lookup "terra" sprites)

-- | Sprite Tile de agua
agua :: ImmutableTowers -> Picture
agua  ImmutableTowers {sprites=sprites} = spriteTranslate $ fromJust (lookup "agua" sprites)

-- | A função 'terrenoToPic' converte tipo de terreno ao respetivo Grafismo
terrenoToPic :: ImmutableTowers -> Terreno -> Picture
terrenoToPic it Relva = relva it
terrenoToPic it Terra = terra it
terrenoToPic it Agua  = agua it



-- Grafismos Construções

-- | Desenho básico de construção
build :: Picture
build = Polygon [(0,0),(32,0),(32,32),(0,32)]

-- | Sprite Portal
portal :: ImmutableTowers -> Picture
portal  ImmutableTowers {sprites=sprites} = Translate 16 40 $ fromJust (lookup "portal" sprites)

-- | Sprite da Base do jogador
base :: ImmutableTowers -> Picture
base  ImmutableTowers {sprites=sprites} = Translate 16 32 $ fromJust (lookup "base" sprites)

-- | Sprite das Torre
torre :: ImmutableTowers -> TipoProjetil -> Picture
torre it tipo = case tipo of
    Fogo -> Translate 18 28 $ fromJust $ lookup "torreFogo" (sprites it)
    Gelo -> Translate 18 28 $ fromJust $ lookup "torreGelo" (sprites it)
    Resina -> Translate 18 28 $ fromJust $ lookup "torreResina" (sprites it)

-- | Sprite de previsão de Torre
torrePrev :: ImmutableTowers -> TipoProjetil -> Picture
torrePrev it tipo = case tipo of
    Fogo -> Translate 18 28 $ fromJust $ lookup "torreFogoPrev" (sprites it)
    Gelo -> Translate 18 28 $ fromJust $ lookup "torreGeloPrev" (sprites it)
    Resina -> Translate 18 28 $ fromJust $ lookup "torreResinaPrev" (sprites it)




-- Funções Para Desenho de Mapa

-- | A função 'drawLine' desenha uma linha do mapa
drawLine :: ImmutableTowers -> [Terreno] -> Float -> [Picture]
drawLine _ [] _ = []
drawLine it (h:t) x = translate (x*tileSize) 0 (terrenoToPic it h) : drawLine it t (x + 1)  -- A distância entre blocos é reduzida para 96 para ajustar a perspectiva

-- | A função 'drawMap' faz o Desenho do mapa total, juntando as linhas 
drawMap :: ImmutableTowers -> Mapa -> (Int, Int) -> [Picture]
drawMap _ [] _ = []
drawMap it (h:t) (x, y) =
    -- Invertemos a direção do Y multiplicando por -1
    translate (fromIntegral x * tileSize) (fromIntegral (-y) * tileSize) (pictures (drawLine it h 0)) : drawMap it t (x, y + 1)

-- | A função 'mapDrawPic' centra o mapa final
mapDrawPic :: ImmutableTowers -> Mapa -> Picture
mapDrawPic it mapa = translate (-(mapWidth/2)) (mapHeight/2-tileSize) $ pictures (drawMap it mapa (0,0))


-- Funções Para Desenho de Construções

-- | A função 'drawBase' faz o desenho da base
drawBase :: ImmutableTowers -> Posicao -> Picture
drawBase it (bx,by) = mapToScreen (centerSprite (bx,by)) (base it)

-- | A função 'drawPortais' faz o desenho dos Portais
drawPortais :: ImmutableTowers -> [Posicao] -> [Picture]
drawPortais it = map (\(px,py) -> mapToScreen (centerSprite (px,py)) (portal it))

-- | A função 'drawTorres' faz o desenho das Torres
drawTorres :: ImmutableTowers -> [Torre] -> [Picture]
drawTorres it = map (\Torre {posicaoTorre=(tx,ty), projetilTorre=projetil} ->
    mapToScreen (centerSprite (tx,ty)) (torre it (tipoProjetil projetil)))


-- | A função 'drawBuild' faz o desenho de todas as contruções
drawBuild :: ImmutableTowers -> Jogo -> Picture
drawBuild it Jogo{baseJogo=baseI, portaisJogo=portais, torresJogo=torres} = Pictures (drawBase it posBase : drawTorres it torres ++ drawPortais it posPortais)
    where posBase     = posicaoBase baseI
          posPortais  = map posicaoPortal portais



-- Funções para Desenho de Inimigos

-- | A função 'getSpriteInimigo' seleciona o sprite de um inimigo dependendo do tipo de inimigo e da respetiva animação
getSpriteInimigo :: ImmutableTowers -> Inimigo -> Int -> Picture
getSpriteInimigo it inimigo frame = case currentAnim of
    Walk   -> case tipoInimigo inimigo of
        Normal -> fromJust (lookup (spriteKey "inimigo1_") (sprites it))
        Rapido -> scale 0.8 0.8 $ fromJust (lookup (spriteKey "inimigo2_") (sprites it))
        Tanque -> fromJust (lookup (spriteKey "inimigo3_") (sprites it))
    Frozen -> fromJust (lookup "frozen" (sprites it))
    where spriteKey x = x ++ show (frame + 1)  -- +1 porque os frames começam em 0 mas sprites em 1
          currentAnim = animacao inimigo


-- | A função 'getSpriteFogo' obtém o sprite do efeito de fogo
getSpriteFogo :: ImmutableTowers -> Int -> Picture
getSpriteFogo it frame = scale 1.1 1.1 $ fromJust $ lookup ("fire" ++ show fireFrame) (sprites it)
    where fireFrame = 1 + (frame `mod` 4)  -- Alterna entre frames 1-4, de forma ciclica ao chegar ao ultimo


-- | A função 'drawInimigo' desenha um único inimigo com animação
drawInimigo :: ImmutableTowers -> Inimigo -> Picture
drawInimigo it inimigo = mapToScreen pos combinedSprite
  where
    pos = centerSprite (ix, iy)
    (ix, iy) = posicaoInimigo inimigo
    baseSprite = Translate 32 32 $ getSpriteInimigo it inimigo (frameInimigo inimigo)
    fireSprite = Translate 32 25 $ getSpriteFogo it (frameInimigo inimigo)
    combinedSprite = if temEfeitoFogo inimigo
                     then Pictures [baseSprite, fireSprite]
                     else baseSprite

-- | A função 'temEfeitoFogo' verifica se um inimigo está sob o efeito de um projetil de fogo
temEfeitoFogo :: Inimigo -> Bool
temEfeitoFogo inimigo = any (\p -> tipoProjetil p == Fogo) (projeteisInimigo inimigo)


-- | A função 'drawInimigos' desenha todos os inimigos
drawInimigos :: ImmutableTowers -> [Inimigo] -> Picture
drawInimigos it = Pictures . map (drawInimigo it)



-- Funções para Desenho de Projeteis

-- Configurações da animação

-- | A função 'combinaPos' calcula uma posição intermédia entre dois pontos, tendo em conta o valor de progresso
combinaPos :: Float -> Posicao -> Posicao -> Posicao
combinaPos progresso (x1,y1) (x2,y2) =
    ( x1 + (x2 - x1) * progresso
    , y1 + (y2 - y1) * progresso
    )

-- | A função 'drawProjetil' desenha um único projétil
drawProjetil :: ImmutableTowers -> ProjetilAtivo -> Picture
drawProjetil it ProjetilAtivo{origemProjetil=origem, destinoProjetil=destino,
                                   progressoProjetil=prog, tipoProjetilAtivo=tipo,
                                   frameAtual=frame} =
    let currentPos = combinaPos prog
            (centerSprite origem)
            (centerSprite destino)
        spriteProjetil = getSpriteProjetil it tipo frame
    in mapToScreen currentPos (Translate 16 16 spriteProjetil)

-- | A função 'drawProjeteis' desenha todos os projéteis ativos
drawProjeteis :: ImmutableTowers -> [ProjetilAtivo] -> Picture
drawProjeteis it projeteis = Pictures $ map (drawProjetil it) projeteis



-- Funções Para Hoover

-- Tile de Hoover

-- | A função 'hooverTile' faz o desenho do efeito de hoovering
hooverTile :: ImmutableTowers -> Picture
hooverTile it@ImmutableTowers {sprites=sprites, buildType=tipo, estadoRato=EstadoRato {tileValid=tileValid}} = case tileValid of
    Nothing -> Blank
    Just False -> Translate 48 48 $ fromJust (lookup "hooverR" sprites)
    Just True  -> Pictures [Translate 48 48 $ fromJust (lookup "hooverB" sprites), torreShadow]
    where torreShadow = translate 32 32 (torrePrev it tipo)


-- | A função 'atualizaHoover' atualiza a posição do efeito de hoover sob a tile selecionada pelo rato
atualizaHoover :: ImmutableTowers -> EstadoRato -> Picture
atualizaHoover it@ImmutableTowers {buildMode=buildMode} EstadoRato {selectedT=pos} = if buildMode
    then case pos of
        Nothing -> Blank
        Just (x,y) -> Translate (x*tileSize-mapWidth/2) (mapHeight/2-(y*tileSize+tileSize)) (hooverTile it)
    else Blank




-- Função para desenhar o nivel atual

-- | A função 'drawLevel' desenha o nível atual
drawLevel :: ImmutableTowers -> Jogo -> Picture
drawLevel it jogo = Pictures [drawInimigos it (inimigosJogo jogo),
                              drawBuild it jogo,
                              drawProjeteis it (projeteisAtivos it)]



-- Funções para desenho de UI

-- | A função 'drawBuildMode' faz o desenho do UI relativo ao modo de construção
drawBuildMode :: ImmutableTowers -> Picture
drawBuildMode it = if buildMode it then Pictures [
    Translate (-800) (-400) $ fromJust $ lookup (getSpriteKey Fogo) (sprites it),
    Translate (-650) (-400) $ fromJust $ lookup (getSpriteKey Gelo) (sprites it),
    Translate (-500) (-400) $ fromJust $ lookup (getSpriteKey Resina) (sprites it)
 ]
 else blank
 where
    getSpriteKey towerType = if buildType it == towerType
                            then show towerType ++ "SelecUI"
                            else show towerType ++ "UI"

-- | A função 'drawSideBar' faz o desenho do UI relativo à side bar
drawSideBar :: ImmutableTowers -> Picture
drawSideBar it = if not (buildMode it) then Pictures [
    Translate 850 180 $ fromJust $ lookup "buildUI" (sprites it)
 ]
 else Translate (-360) (-480) $ fromJust $ lookup "returnBUI" (sprites it)


-- | A função 'drawNum' faz o desenho de qualquer número, decompondo-o 
drawNum :: Int -> String -> (Float, Float) -> ImmutableTowers -> Picture
drawNum n colorS (x,y) it = Pictures $ foldl (\p c -> p ++ [Translate (x + 30*fromIntegral (length p)) y ( scale 0.9 0.9 $
    fromJust (lookup (colorS ++ [c]) (sprites it)))]) [] (show n)


-- | A função 'drawStats' faz o desenho do UI relativo às Stats do jogador
drawStats :: ImmutableTowers -> Picture
drawStats it = Pictures [
    Translate (-780) 475 $ fromJust $ lookup "healthUI" (sprites it), drawNum (round vida) "white" (-775,475) it,
    Translate 0 475 $ fromJust $ lookup "waveUI" (sprites it), Translate 55 469 $ scale 0.7 0.7 $ drawNum (ondasLeft it) "white" (0,0) it,
    Translate 750 475 $ fromJust $ lookup "creditosUI" (sprites it), drawNum creditos "creditos" (700, 475) it
    ]

    where level = currentLevel it
          creditos = creditosBase (baseJogo level)
          vida = vidaBase (baseJogo level)


-- | A função 'drawUI' faz o desenho do UI geral
drawUI :: ImmutableTowers -> Picture
drawUI it = Pictures [drawBuildMode it, drawSideBar it, drawStats it]








-- Funções de Desenho de Menus:

-- | A função 'drawMenu' gere o desenho de cada menu
drawMenu :: ImmutableTowers -> IO Picture
drawMenu it = case currentMenu it of
    MainMenu        -> drawMainMenu it
    LevelSelection  -> drawLevelSelection it
    Tutorial        -> drawTutorial it
    GameOver        -> drawGameover it
    GameWin x       -> drawGameWin it x
    _               -> return blank


-- | A função 'menuArrowsLimit' define o limite de butões de cada menu
menuArrowsLimit :: ImmutableTowers -> Int
menuArrowsLimit it@ImmutableTowers {currentLevelIndex=levelI} = case currentMenu it of
    MainMenu -> 2 -- Play, Tutorial, Exit
    LevelSelection -> 5 -- Número de níveis + botão "Voltar"
    InGame -> 0
    GameOver -> 1 -- replay e level selection
    GameWin _ -> if levelI < 5 then 2 else 1
    _ -> 3 -- Caso geral


-- | A função 'drawMainMenu' faz o desenho do menu principal
drawMainMenu :: ImmutableTowers -> IO Picture
drawMainMenu it = do
    let bgMenu = fromJust $ lookup "bgMenu" (sprites it)
        playButton = fromJust $ lookup "playButton" (sprites it)
        playButtonSelected = fromJust $ lookup "playButtonSelected" (sprites it)
        tutorialButton = fromJust $ lookup "tutorialButton" (sprites it)
        tutorialButtonSelected = fromJust $ lookup "tutorialButtonSelected" (sprites it)
        exitButton = fromJust $ lookup "exitButton" (sprites it)
        exitButtonSelected = fromJust $ lookup "exitButtonSelected" (sprites it)
        keysHelp = fromJust $ lookup "keysHelp" (sprites it)

    let buttons = Pictures [
            translate 0 (-100) (if selectedButton (menuState it) == 0 then playButtonSelected else playButton),
            translate 0 (-210) (if selectedButton (menuState it) == 1 then tutorialButtonSelected else tutorialButton),
            translate 0 (-310) (if selectedButton (menuState it) == 2 then exitButtonSelected else exitButton ),
            translate 840 (-430) keysHelp
            ]
    return $ Pictures [bgMenu, buttons]


-- | A função 'drawTutorial' faz o desenho do menu de tutorial
drawTutorial :: ImmutableTowers -> IO Picture
drawTutorial it = do
    let bgLS = fromJust $ lookup "bgLS" (sprites it)
        exitButtonSelected = fromJust $ lookup "exitButtonSelected" (sprites it)
        keysHelp = fromJust $ lookup "keysHelp" (sprites it)

        currentTutorialSlide = fromJust $ lookup ("tutorial" ++ show (currentSlide (tutorialState it) +1)) (sprites it)

    let content = Pictures [
            translate (-840) (-470) exitButtonSelected,
            translate 855 (-430) keysHelp
            ]
    return $ Pictures [bgLS, currentTutorialSlide, content]


-- | A função 'drawLevelSelection' faz o desenho do menu de level selection
drawLevelSelection :: ImmutableTowers -> IO Picture
drawLevelSelection it = do
    let bgLS = fromJust $ lookup "bgLS" (sprites it)
        lockLevel = fromJust $ lookup "lockLevel" (sprites it)
        lockLevelSelected = fromJust $ lookup "lockLevelSelec" (sprites it)
        level1 = fromJust $ lookup "level1" (sprites it)
        level1S = fromJust $ lookup "level1S" (sprites it)
        level2 = fromJust $ lookup "level2" (sprites it)
        level2S = fromJust $ lookup "level2S" (sprites it)
        level3 = fromJust $ lookup "level3" (sprites it)
        level3S = fromJust $ lookup "level3S" (sprites it)
        level4 = fromJust $ lookup "level4" (sprites it)
        level4S = fromJust $ lookup "level4S" (sprites it)
        level5 = fromJust $ lookup "level5" (sprites it)
        level5S = fromJust $ lookup "level5S" (sprites it)
        backBtn = fromJust $ lookup "backBtn" (sprites it)
        backBtnS = fromJust $ lookup "backBtnS" (sprites it)

    let buttons = Pictures [
            translate (-300) 100 (if selectedButton (menuState it) == 0 then level1S else level1),

            translate (-150) 100
            (if selectedButton (menuState it) == 1
            then if snd (levels it !! 1) then level2S else lockLevelSelected
            else if not (snd (levels it !! 1)) then lockLevel else level2),

            translate 0 100
            (if selectedButton (menuState it) == 2
            then if snd (levels it !! 2) then level3S else lockLevelSelected
            else if not (snd (levels it !! 2)) then lockLevel else level3),

            translate 150 100
            (if selectedButton (menuState it) == 3
            then if snd (levels it !! 3) then level4S else lockLevelSelected
            else if not (snd (levels it !! 3)) then lockLevel else level4),

            translate 300 100
            (if selectedButton (menuState it) == 4
            then if snd (levels it !! 4) then level5S else lockLevelSelected
            else if not (snd (levels it !! 4)) then lockLevel else level5),

            translate (-780) (-400) (if selectedButton (menuState it) == 5 then backBtnS else backBtn)

            ]

    return $ Pictures [bgLS, buttons]


-- | A função 'drawGameover' faz o desenho do menu Game Over
drawGameover :: ImmutableTowers -> IO Picture
drawGameover it = do
    let gameoverBG = fromJust $ lookup "gameoverBG" (sprites it)
        levelMenuButton = fromJust $ lookup "levelMenuButton" (sprites it)
        levelMenuButtonSelected = fromJust $ lookup "levelMenuButtonSelected" (sprites it)
        restartButton = fromJust $ lookup "restartButton" (sprites it)
        restartButtonSelected = fromJust $ lookup "restartButtonSelected" (sprites it)

    let buttons = Pictures [
            translate (-75) (-40) (if selectedButton (menuState it) == 0 then restartButtonSelected else restartButton),
            translate 75 (-40) (if selectedButton (menuState it) == 1 then levelMenuButtonSelected else levelMenuButton)
            ]
    return $ Pictures [gameoverBG, buttons]


-- | A função 'drawGameWin' faz o desenho do menu Game Win
drawGameWin :: ImmutableTowers -> Int -> IO Picture
drawGameWin it@ImmutableTowers {currentLevelIndex=levelI} vida = do
    let gamewinBG
          | vida == 100 = fromJust $ lookup "gamewinBG3" (sprites it)
          | vida >= 50  = fromJust $ lookup "gamewinBG2" (sprites it)
          | otherwise   = fromJust $ lookup "gamewinBG1" (sprites it)

        restartButton = fromJust $ lookup "restartButton" (sprites it)
        restartButtonSelected = fromJust $ lookup "restartButtonSelected" (sprites it)
        levelMenuButton = fromJust $ lookup "levelMenuButton" (sprites it)
        levelMenuButtonSelected = fromJust $ lookup "levelMenuButtonSelected" (sprites it)
        playMini = fromJust $ lookup "playMini" (sprites it)
        playMiniSelected = fromJust $ lookup "playMiniSelected" (sprites it)


    let buttons
            | levelI <5 = Pictures [
                translate (-120) (-200) (if selectedButton (menuState it) == 0 then restartButtonSelected else restartButton),
                translate 0 (-200) (if selectedButton (menuState it) == 1 then playMiniSelected else playMini),
                translate 120 (-200) (if selectedButton (menuState it) == 2 then levelMenuButtonSelected else levelMenuButton),
                drawNum vida "white" (-25, -100) it
                ]
            | otherwise = Pictures [
                translate (-60) (-200) (if selectedButton (menuState it) == 0 then restartButtonSelected else restartButton),
                translate 60 (-200) (if selectedButton (menuState it) == 1 then levelMenuButtonSelected else levelMenuButton),
                drawNum vida "white" (-25, -100) it
                ]
    return $ Pictures [gamewinBG, buttons]