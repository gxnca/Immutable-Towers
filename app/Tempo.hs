module Tempo where

import ImmutableTowers
import LI12425
import Tarefa2 (ganhouJogo, perdeuJogo)
import Tarefa3 (atualizaJogo)
import Save
import Niveis

-- | A função 'reageTempo' é a função principal de atualização de jogo com o decorrer do tempo
reageTempo :: Tempo -> ImmutableTowers -> IO ImmutableTowers
reageTempo tempo it@ImmutableTowers{currentLevel=level, currentLevelIndex=levelIndex, sprites=spritesLoaded, currentMenu=menu} = case menu of
    InGame -> if ganhouJogo level 
                then do 
                    novoIt <- afterWinningLevel levelIndex it
                    return novoIt { currentMenu=GameWin vida, 
                                    buildMode=False, 
                                    buildType=Fogo, 
                                    ondasLeft=0, 
                                    estadoRato=ratoI, 
                                    projeteisAtivos=[] }
              
              else if perdeuJogo level 
                then return itReset
                else return it { currentLevel=atualizaJogo tempo level {inimigosJogo=inimigosAnimados}, 
                                 projeteisAtivos = novosProjeteis ++ projeteisAtualizados, 
                                 ondasLeft=ondasAtivas }
    
    _ -> return it 
    
    where
        projeteisAtualizados = atualizaProjeteis tempo (projeteisAtivos it)
        novosProjeteis = concatMap (\t -> criaProjeteis t (inimigosJogo level)) (torresJogo level)
        inimigosAnimados = atualizaAnimacaoInimigos tempo (inimigosJogo level)
        ondas = concatMap ondasPortal (portaisJogo level)
        ondasAtivas = length $ filter (\o -> not (null $ inimigosOnda o)) ondas

        vida = round $ vidaBase (baseJogo level)

        itReset = it {currentMenu=GameOver, menuState= MenuState {selectedButton= 0, pressingButton= False}, buildMode=False, buildType=Fogo, currentLevel=nivel1, ondasLeft=0 , estadoRato=ratoI, sprites=spritesLoaded, projeteisAtivos=[]}
        ratoI = EstadoRato {posRato=(0,0), selectedT=Nothing, tileValid=Nothing}


-- | A função 'atualizaLevels' é a função responsável por desbloquear o nível seguinte
atualizaLevels :: Levels -> Int -> Levels
atualizaLevels niveis n =
        [if i == n + 1 then (nivel, True) else (nivel, estado) | (i, (nivel, estado)) <- zip [1..] niveis]

-- | A função 'afterWinningLevel' atualiza e guarda a nova lista de niveis
afterWinningLevel :: Int -> ImmutableTowers -> IO ImmutableTowers
afterWinningLevel levelIndex it = do
    let newLevels = atualizaLevels (levels it) levelIndex
    saveProgress newLevels
    return it { levels = newLevels }



-- ANIMAÇÕES:

    -- Animação Inimigos:

-- Configurações

-- | FrameRate Inimigo do tipo Normal
frameRateInimigo1 :: Float
frameRateInimigo1 = 0.04  -- Tempo entre frames

-- | FrameRate Inimigo do tipo Rapido
frameRateInimigo2 :: Float
frameRateInimigo2 = 0.02

-- | FrameRate Inimigo do tipo Tanque
frameRateInimigo3 :: Float
frameRateInimigo3 = 0.08

-- | Número máximo de frames na animação de um inimigo
maxFramesInimigo :: Int
maxFramesInimigo = 20    -- Número de frames na animação


-- | A função 'atualizaAnimacaoInimigos' atualiza a animação dos inimigos
atualizaAnimacaoInimigos :: Float -> [Inimigo] -> [Inimigo]
atualizaAnimacaoInimigos dt = map atualizaAnimacao
  where
    atualizaAnimacao inimigo =
        let novoTempo = tempoAInimigo inimigo + dt
            frameRate = case tipoInimigo inimigo of     -- verifica qual o inimigo para que tenha a respetiva framerate
                    Normal -> frameRateInimigo1
                    Rapido -> frameRateInimigo2
                    Tanque -> frameRateInimigo3

            (novoFrame, tempoRestante) =
                if novoTempo >= frameRate
                then ((frameInimigo inimigo + 1) `mod` maxFramesInimigo, novoTempo - frameRate) -- assim que atinge o ultimo sprite, volta ao inicio
                else (frameInimigo inimigo, novoTempo)

        in inimigo { frameInimigo = novoFrame,
                     tempoAInimigo = tempoRestante
                   }



    -- Animação Projeteis:

-- Configurações da animação

-- | FrameRate de um projetil
frameRateProjetil :: Float
frameRateProjetil = 0.1  -- Tempo em segundos para cada frame

-- | Número máximo de frames na animação de um projetil
maxFramesProjetil :: Int
maxFramesProjetil = 5    -- Número de frames em cada animação


-- | A função 'atualizaProjeteis' atualiza a animação dos Projeteis
atualizaProjeteis :: Float -> [ProjetilAtivo] -> [ProjetilAtivo]
atualizaProjeteis dt = filter (\p -> progressoProjetil p < 1.0) . map atualizaProjetil
  where
    atualizaProjetil proj =
        let novoTempo = tempoFrame proj + dt
            (novoFrame, tempoRestante) = if novoTempo >= frameRateProjetil
                                       then ((frameAtual proj + 1) `mod` maxFramesProjetil, novoTempo - frameRateProjetil) -- assim que atinge o ultimo sprite, volta ao inicio
                                       else (frameAtual proj, novoTempo)
        in proj { progressoProjetil = progressoProjetil proj + (dt * velocidadeProjetil)
                , frameAtual = novoFrame
                , tempoFrame = tempoRestante
                }
    velocidadeProjetil = 4


-- | A função 'estaEmAlcance' verifica se um inimigo esta no alcance da torre
estaEmAlcance :: Torre -> Inimigo -> Bool
estaEmAlcance torre inimigo =
    let (tx,ty) = posicaoTorre torre
        (ix,iy) = posicaoInimigo inimigo
        dx = tx - ix
        dy = ty - iy
        distancia = sqrt (dx * dx + dy * dy)
    in distancia <= alcanceTorre torre

-- | A função 'criaProjeteis' cria novos projéteis quando as torres disparam
criaProjeteis :: Torre -> [Inimigo] -> [ProjetilAtivo]
criaProjeteis torre inimigos
    | tempoTorre torre <= 0 && not (null inimigosAlcance) =
        take (rajadaTorre torre) $ map criaProjetil inimigosAlcance
    | otherwise = []
  where
    inimigosAlcance = filter (estaEmAlcance torre) inimigos
    criaProjetil inimigo = ProjetilAtivo
        { origemProjetil = posicaoTorre torre
        , destinoProjetil = posicaoInimigo inimigo
        , tipoProjetilAtivo = tipoProjetil (projetilTorre torre)
        , progressoProjetil = 0.0
        , frameAtual = 1
        , tempoFrame = 0.0
        }
