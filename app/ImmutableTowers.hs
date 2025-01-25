module ImmutableTowers where

import LI12425
import Graphics.Gloss (Picture)

-- | Data type principal de estado de jogo
data ImmutableTowers = ImmutableTowers { currentMenu :: Menu,
                                         menuState :: MenuState,
                                         tutorialState :: TutorialState,


                                         buildMode :: Bool,
                                         buildType :: TipoProjetil,

                                         levels :: Levels,
                                         currentLevel :: Jogo,
                                         currentLevelIndex :: Int,
                                         ondasLeft :: Int,

                                         estadoRato :: EstadoRato,

                                         sprites :: [Sprite],
                                         projeteisAtivos :: [ProjetilAtivo]

                                       }
-- | Tipos de menus disponiveis
data Menu = InGame | MainMenu | LevelSelection | GameOver | GameWin Int | Tutorial deriving (Eq)

-- | Estado do menu
data MenuState = MenuState { selectedButton :: Int,
                             pressingButton :: Bool
                           }

-- | Estado dos diapositivos do menu de tutorial
data TutorialState = TutorialState { selectedArrow :: Int,
                                     currentSlide :: Int
                                   }

-- | Armazenamento de níveis
type Levels = [(Jogo, Bool)]

-- | Sprite do jogo
type Sprite = (String, Picture) 


-- | Estado do rato, utilizado para o sistema de hoovering em tiles
data EstadoRato = EstadoRato {posRato::Posicao,             -- armazena as coordenadas do rato
                              selectedT::Maybe Posicao,      -- armazena uma tile selecionada
                              tileValid :: Maybe Bool
                             }


-- | Data type relativo ao desenho de projeteis
data ProjetilAtivo = ProjetilAtivo
    { origemProjetil :: Posicao             
    , destinoProjetil :: Posicao            
    , tipoProjetilAtivo :: TipoProjetil     
    , progressoProjetil :: Float            
    , frameAtual :: Int                     
    , tempoFrame :: Float                   
    } deriving (Show, Eq)

