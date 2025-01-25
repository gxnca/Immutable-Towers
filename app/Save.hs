module Save where 

import LI12425
import System.Directory (doesFileExist)

filePath :: FilePath
filePath = "./saves/progress.txt"                            -- permite guardar na pasta saves

-- Função para Save de Progresso
saveProgress :: [(Jogo, Bool)] -> IO ()
saveProgress levels = 
    writeFile filePath (unwords $ map (show . snd) levels)   -- extrai e salva o Bool de cada Nivel


-- Função para carregar progresso
loadProgress :: [(Jogo, Bool)] -> IO [(Jogo, Bool)]
loadProgress defaultLevels = do
    exists <- doesFileExist filePath                         -- verifica se existe um ficheiro de save
    if exists
        then do
            content <- readFile filePath
            let unlocked = map read (words content) :: [Bool]
            return $ zip (map fst defaultLevels) unlocked    -- associa os niveis aos dados carregados do save
        else return defaultLevels                            -- se não existe save, então começa com dados default

