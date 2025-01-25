module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import Tempo
import Niveis
import LI12425
import Save

import Graphics.Gloss.Interface.IO.Game (playIO)



-- | Janela do jogo
janela :: Display
janela = InWindow "Immutable Towers" (1920, 1080) (0, 0)

-- | Fundo da Janela do jogo
fundo :: Color
fundo = makeColorI 70 145 11 255

-- | FrameRate do jogo
fr :: Int
fr = 60

-- | Função Principal para rodar o jogo
main :: IO ()
main = do
  putStrLn "Hello from Immutable Towers!"

  savedLevels <- loadProgress (levels it)
  let initialState = it { levels = savedLevels }
    
  itT <- getSprites initialState   

  playIO janela fundo fr itT desenha reageEventos reageTempo
  where
    it = ImmutableTowers { currentMenu=MainMenu, 
                           menuState= MenuState {selectedButton= 0, pressingButton= False}, 
                           tutorialState= TutorialState {selectedArrow= 0, currentSlide=0}, 
                           buildMode=False, 
                           buildType=Fogo, 
                           levels=levelsDefault, 
                           currentLevel=nivel1, 
                           currentLevelIndex=1, 
                           ondasLeft=0 , 
                           estadoRato=ratoI, 
                           sprites=[], 
                           projeteisAtivos=[]
                         }
    
    -- estado inicial do rato
    ratoI = EstadoRato { posRato=(0,0),
                         selectedT=Nothing, 
                         tileValid=Nothing 
                       }   

-- | Valor default dos níveis
levelsDefault :: Levels
levelsDefault = [(nivel1,True),(nivel2,False),(nivel3,False),(nivel4,False),(nivel5,False)]

