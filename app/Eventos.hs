module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import Desenhar (mapWidth, tileSize, mapHeight, menuArrowsLimit)
import System.Exit (exitSuccess)

import Debug.Trace
import LI12425

-- | A função 'reageEventos' é a função principal de recebimento de inputs do player
reageEventos :: Event -> ImmutableTowers -> IO ImmutableTowers

reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess                  --TODO: usado para debug

reageEventos evento it@ImmutableTowers  {currentMenu=MainMenu} = eventHandlerInMenu evento it
reageEventos evento it@ImmutableTowers  {currentMenu=Tutorial} = eventHandlerInMenu evento it
reageEventos evento it@ImmutableTowers  {currentMenu=LevelSelection} = eventHandlerInMenu evento it
reageEventos evento it@ImmutableTowers  {currentMenu=GameOver} = eventHandlerInMenu evento it
reageEventos evento it@ImmutableTowers  {currentMenu=GameWin _} = eventHandlerInMenu evento it



-- COMANDOS DE CHEATS:
    -- dinheiro infinito
reageEventos (EventKey (SpecialKey KeyF4) Down (Modifiers {ctrl=Down}) _) it@ImmutableTowers {currentLevel = j@Jogo {baseJogo = b@Base {creditosBase=creditos}}} = return it {currentLevel = j {baseJogo = b {creditosBase = creditos + 9999}}}
    -- instant kill em todos os inimigos em jogo
reageEventos (EventKey (SpecialKey KeyF1) Down (Modifiers {ctrl=Down}) _) it@ImmutableTowers {currentLevel = j} = return it {currentLevel = j {inimigosJogo = []}}
    -- limpa todas as ondas (remove inimigos)
reageEventos (EventKey (SpecialKey KeyF2) Down (Modifiers {ctrl=Down}) _) it@ImmutableTowers {currentLevel = j@Jogo {portaisJogo = portais}} = return it {currentLevel = j {portaisJogo = ondasClean}}
  where ondasClean = map (\portal -> portal {ondasPortal = map (\onda -> onda {inimigosOnda = []}) (ondasPortal portal)}) portais



-- Ativar/Desativar modo Construção
reageEventos (EventKey (Char 'b') Down _ _) i@ImmutableTowers {estadoRato = e} =
   if buildMode i then return i {buildMode=False, estadoRato=e {selectedT=Nothing}} -- tileSelecionada = Nothing para evitar desenho antes de movimentar o rato
                  else return i {buildMode=True}

-- Seleção da Torre a ser construida
reageEventos (EventKey (Char '1') Down _ _) i = return i {buildType=Fogo}
reageEventos (EventKey (Char '2') Down _ _) i = return i {buildType=Gelo}
reageEventos (EventKey (Char '3') Down _ _) i = return i {buildType=Resina}


-- Seleção de tiles e Construção
reageEventos (EventKey (MouseButton LeftButton) Down _ _) i = placeOnTile i
reageEventos evento i@ImmutableTowers{buildMode=True}= mouseHoover evento i


-- caso geral                                                                  
reageEventos _ x = return x


-- | A função 'mouseHoover' é responsável por reagir ao hoovering do jogador através do rato
mouseHoover :: Event -> ImmutableTowers -> IO ImmutableTowers
mouseHoover (EventMotion (mouseX, mouseY)) it@ImmutableTowers{currentLevel=level, estadoRato=rato, buildType=typeT} = 
--    trace ("Mx: " ++ show mouseX ++ "\n" ++ "My: " ++ show mouseY) $   -- linhas para debug (coordendas rato)
--    trace ("Ax: " ++ show x ++ "\n" ++ "Ay: " ++ show y) $             -- linhas para debug (coordenadas tratadas)
--    trace ("Tile" ++ show tile) $                                      -- linhas para debug

    case tile of

  Nothing -> return it{estadoRato= rato{posRato=(mouseX,mouseY), selectedT=tile, tileValid=Nothing}}
  Just (tx, ty) -> 
            if yIndex < 0 || yIndex >= length mapa || xIndex < 0 || xIndex >= length (mapa !! yIndex)   -- condições para evitar crash !! index too large (limita dimensões do mapa)
            then return it  -- retorna o mesmo it por estar fora dos limites
            else case terreno of
                Relva -> if naoSobrepoe (tx,ty) level && temCreditos loja (baseJogo level) typeT
                          then return it{estadoRato = rato{posRato = (mouseX, mouseY), selectedT = tile, tileValid = Just True}}
                          else return it{estadoRato = rato{posRato = (mouseX, mouseY), selectedT = tile, tileValid = Just False}}
                Terra -> return it{estadoRato = rato{posRato = (mouseX, mouseY), selectedT = tile, tileValid = Just False}}
                Agua  -> return it{estadoRato = rato{posRato = (mouseX, mouseY), selectedT = tile, tileValid = Just False}}

    where terreno = (mapa !! yIndex) !! xIndex
          yIndex = floor ty
          xIndex = floor tx
          loja = lojaJogo level
          mapa = mapaJogo level

          


  where -- (x,y) = mouseToMap (mouseX, mouseY)                         -- usado no debug
        tile = mouseToTile (mouseX,mouseY)

mouseHoover _ x = return x


-- | A função 'mouseToMap' converte uma coordenada do rato, numa coordenada relativa ao mapa
mouseToMap :: (Float,Float) -> (Float,Float)
mouseToMap (mx,my) =
  (x,y)
  where x = mx + (mapWidth/2)
        y = (mapHeight/2) - my

-- | A função 'mouseToTile' converte uma coordenada do rato numa tile, se possível
mouseToTile :: (Float,Float) -> Maybe (Float,Float)
mouseToTile (mx,my)
  | mx >=(-limitX) && mx <= limitX && my <=limitY && my >= (-limitY) = Just (x,y)
  | otherwise = Nothing
  where (adjX,adjY) = mouseToMap (mx,my)
        x = fromIntegral (floor (adjX / tileSize)::Int)
        y = fromIntegral (floor (adjY / tileSize)::Int)
        limitX = mapWidth/2  -- 288 para tiles de 96px
        limitY = mapHeight/2


-- | A função 'placeOnTile' é responsável por tratar da colocação de torres numa tile selecionada
placeOnTile :: ImmutableTowers -> IO ImmutableTowers
placeOnTile i@ImmutableTowers {estadoRato=rato, buildType=typeT, currentLevel=level} = case selTile of
  Nothing -> return i
  Just (x,y) -> case terreno of
    Relva -> (if naoSobrepoe (x,y) level && temCreditos loja (baseJogo level) typeT
      then trace ("Torre adicionada em" ++ show (x,y))        -- linha para debug
          return i {estadoRato=rato, currentLevel=(j {torresJogo= torreNova : torres, baseJogo=b {creditosBase= creditosI - price}})}

      else return i)

    Terra -> return i
    Agua  -> return i
    where terreno = (mapaJogo level !! floor y) !! floor x
          loja = lojaJogo level
          (price, torreNova) = getFromLoja loja typeT (x,y)

  where selTile = selectedT rato
        j@Jogo{torresJogo=torres} = level
        b@Base{creditosBase=creditosI} = baseJogo level


-- | A função 'naoSobrepes' verifica se uma torre não está a sobrepor uma outra
naoSobrepoe :: Posicao -> Jogo -> Bool
naoSobrepoe (x,y) Jogo {torresJogo=torres} = not (any (\(tx,ty) -> (tx,ty)==(x,y)) posTorres)
  where posTorres = map posicaoTorre torres

-- | A função 'temCreditos' verifica se o jogador tem creditos para colocar a torre
temCreditos :: Loja -> Base -> TipoProjetil -> Bool
temCreditos loja Base {creditosBase=creditos} tipo =
  any (\(price, torre) -> tipoProjetil (projetilTorre torre) == tipo && creditos >= price) loja

-- | A função 'getFromLoja' retira os dados da loja para cada Torre
getFromLoja :: Loja -> TipoProjetil -> Posicao -> (Creditos, Torre)
getFromLoja loja tipo (x,y) = (price, torreL {posicaoTorre=(x,y)})
  where (price, torreL)= head (filter (\(_, torre) -> tipoProjetil (projetilTorre torre) == tipo) loja)




-- Relativo as Menus

-- | A função 'eventHandlerInMenu' é responsável por todos os inputs no menus
eventHandlerInMenu :: Event -> ImmutableTowers -> IO ImmutableTowers
eventHandlerInMenu (EventKey (SpecialKey KeyDown) Down _ _) it =
    let
        maxBtn = menuArrowsLimit it
        newSelected = (selectedButton (menuState it) + 1) `mod` (maxBtn + 1)
    in
        return $ it { menuState = (menuState it) { selectedButton = newSelected } }

eventHandlerInMenu (EventKey (SpecialKey KeyUp) Down _ _) it =
    let
        maxBtn = menuArrowsLimit it
        newSelected = (selectedButton (menuState it) - 1 + maxBtn + 1) `mod` (maxBtn + 1)
    in
        return $ it { menuState = (menuState it) { selectedButton = newSelected } }

eventHandlerInMenu (EventKey (SpecialKey KeyEnter) Down _ _) it =
    case currentMenu it of
        LevelSelection -> buttonPress it
        _              -> buttonPress it

-- Para o menu tutorial:      
eventHandlerInMenu (EventKey (SpecialKey KeyLeft) Down _ _) it =
    let
        maxSlides = 3 -- o index começa em 0, logo -1
        currentSlideNum = currentSlide (tutorialState it)
        newSlide = if currentSlideNum == 0
                   then maxSlides  -- Se estiver no primeiro, vai para o último
                   else currentSlideNum - 1
    in
        return $ it { tutorialState = (tutorialState it) { currentSlide = newSlide } }

eventHandlerInMenu (EventKey (SpecialKey KeyRight) Down _ _) it =
    let
        maxSlides = 3
        currentSlideNum = currentSlide (tutorialState it)
        newSlide = if currentSlideNum == maxSlides
                   then 0  -- Se estiver no último, volta para o primeiro
                   else currentSlideNum + 1
    in
        return $ it { tutorialState = (tutorialState it) { currentSlide = newSlide } }

eventHandlerInMenu _ it = return it


-- | A função 'buttonPress' executa a função correspondente quando um determinado botão é escolhido
buttonPress :: ImmutableTowers -> IO ImmutableTowers
buttonPress it@ImmutableTowers {currentLevelIndex=levelI} = case currentMenu it of
    MainMenu -> case selectedButton (menuState it) of
        0 -> return it { currentMenu = LevelSelection, menuState= (menuState it) {selectedButton=0} } -- botão "Play"
        1 -> return it { currentMenu = Tutorial, tutorialState= (tutorialState it) {currentSlide=0} }       -- botão "Tutorial"
        2 -> exitSuccess                                -- botão "Exit"
        _ -> return it

    Tutorial -> case selectedButton (menuState it) of
        1 -> return it { currentMenu = MainMenu }       -- botão "Voltar ao Menu Principal"
        _ -> return it

    LevelSelection -> case selectedButton (menuState it) of
        5 -> return it { currentMenu = MainMenu }       -- botão "Voltar"
        _ -> if snd (levels it !! selectedButton (menuState it))
             then return it { currentMenu = InGame, currentLevel = fst (levels it !! selectedButton (menuState it)), currentLevelIndex=selectedButton (menuState it) + 1 }
             else return it                             -- mantém o it se o nível estiver bloqueado

    GameOver -> case selectedButton (menuState it) of
        0 -> return it { currentMenu = InGame, currentLevel = fst (levels it !! (currentLevelIndex it - 1)), currentLevelIndex= currentLevelIndex it}
        1 -> return it { currentMenu = LevelSelection, menuState= (menuState it) {selectedButton= currentLevelIndex it-1 } }
        _ -> return it

    GameWin _ -> if levelI < 5 then case selectedButton (menuState it) of
        0 -> return it { currentMenu = InGame, currentLevel = fst (levels it !! (currentLevelIndex it - 1)), currentLevelIndex= currentLevelIndex it}
        1 -> return it { currentMenu = InGame, currentLevel = fst (levels it !! currentLevelIndex it ), currentLevelIndex=currentLevelIndex it + 1}
        2 -> return it { currentMenu = LevelSelection, menuState= (menuState it) {selectedButton= currentLevelIndex it }}
        _ -> return it
                 else case selectedButton (menuState it) of
        0 -> return it { currentMenu = InGame, currentLevel = fst (levels it !! (currentLevelIndex it - 1)), currentLevelIndex= currentLevelIndex it}
        1 -> return it { currentMenu = LevelSelection, menuState= (menuState it) {selectedButton= currentLevelIndex it }}
        _ -> return it

    _ -> return it
