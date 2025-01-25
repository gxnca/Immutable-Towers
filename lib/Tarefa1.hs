{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : Gonçalo José Arantes Fernandes <a111855@alunos.uminho.pt>
              Felipe Youssef Braga da Silva <a109385@alunos.uminho.pt>
Maintainer  : a111855@alunos.uminho.pt


Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}
module Tarefa1 where

import LI12425
import Data.List





{-|A função 'validaJogo' é responsável por fazer todas a verificações necessárias para que um jogo seja válido.

==__Exemplos de Aplicação:__
>>> validaJogo jogoEx
True

==__Funções Auxiliares:__
* Função 'validaPortais'
* Função 'validaInimigosAll'
* Função 'projeteisNorm'
* Função 'validaTorres'
* Função 'validaBase'

-}
validaJogo :: Jogo                           -- ^ recebe o jogo
                 -> Bool                     -- ^ devolve um Booleano
validaJogo Jogo {baseJogo=baseJogo@Base {posicaoBase=posBase}, portaisJogo=portaisJogo, torresJogo=torresJogo, mapaJogo=mapaJogo, inimigosJogo=inimigosJogo} =  validaPortais portaisJogo mapaJogo posBase torresJogo                                   -- valida os Portais em jogo
                                                                                                                                                             && validaInimigosAll portaisJogo mapaJogo torresJogo inimigosJogo                          -- valida os inimigos que ainda não deram spawn
                                                                                                                                                             && all (\Inimigo {projeteisInimigo=projeteis} -> projeteisNorm projeteis) inimigosJogo     -- valida os inimigos já spawnados
                                                                                                                                                             && validaTorres torresJogo mapaJogo                                                        -- valida as Torres em jogo
                                                                                                                                                             && validaBase baseJogo mapaJogo portaisJogo torresJogo                                     -- valida a Base do Jogador


-- Validação de Portáis:


{-|A função 'validaPortais' é responsável por fazer todas a verificações, __relativas a portais__, necessárias
para que um jogo seja válido. Esta função é utilizada pela função 'validaJogo'. 

==__Exemplos de Aplicação:__
>>> validaPortais [] mapa01 (3,2) torresEx
False

>>> validaPortais [(Portal (1,1) ondaEx)] mapa01 (3,4) torresEx
True
-}
validaPortais :: [Portal]                    -- ^ recebe a lista de portais em jogo
                    -> Mapa                  -- ^ recebe o mapa do jogo
                        -> Posicao           -- ^ recebe a posição da base
                            -> [Torre]       -- ^ recebe a lista de torres em jogo                                          //todo: não devia verificar se portal sobrepoe portal?
                                    -> Bool  -- ^ devolve um Bool
validaPortais [] _ _ _ = False                                                                                    -- | verifica se existe pelo menos um portal
validaPortais portais mapa base torres =  portaisTerra posPortais mapa                                            -- | verifica se os portais estão sobre terra
                                       && emTerra base mapa                                                       -- | verifica se a base está sobre terra
                                       && all (\(x,y) -> checkCaminho mapa (x,y) base []) posPortais              -- | verifica se existe um caminho em terra que ligue o portal e a base
                                       && not (all (\(x,y) -> portalSobrepoe (x,y) base posTorres) posPortais)    -- | verifica se nenhum dos portais sobrepoe a base ou uma Torre
                                       && all umaOndaAtiva portais                                                -- | verifica se cada portal tem apenas uma onda ativa
    where posPortais = map posicaoPortal portais                                                          -- | extrai todas as posições dos Portais para uma lista
          posTorres  = map posicaoTorre torres


{-|A função 'portaisTerra' é responável por verificar se todos os portais de uma lista de portais estão sobre o terreno de Terra.
Utiliza a função 'emTerra'.

==__Exemplos de Aplicação:__
>>>portaisTerra [(1,1)] mapa01
True
-}
portaisTerra :: [Posicao]                    -- ^ recebe a lista de portais em jogo
                    -> Mapa                  -- ^ recebe o mapa do jogo
                        -> Bool              -- ^ devolve um Booleano
portaisTerra portais mapa = all (\(x,y) -> emTerra (x,y) mapa ) portais


{-|A função 'emTerra' é responável por verificar se um dado bloco de coordenada (x,y) é um terreno de Terra.

==__Exemplos de Aplicação:__
>>>emTerra (1,1) mapa01
True
-}
emTerra :: Posicao                           -- ^ recebe a posição de um determinado bloco
             -> Mapa                         -- ^ recebe o mapa do jogo
                -> Bool                      -- ^ devolve um Booleano
emTerra (x,y) mapa = not (yIndex < 0 || yIndex >= length mapa || xIndex < 0 || xIndex >= length (mapa !! yIndex)) && ((mapa !! yIndex) !! xIndex == Terra)
    where xIndex = floor x
          yIndex = floor y


{-|A função 'dentroMapa' é responsável por verificar se uma bloco de coordenada (x,y) pertence ou não ao mapa de jogo

==__Exemplos de aplicação:__
>>> dentroMapa mapa01 (0,0)
True

>>> dentroMapa mapa01 (20,54)
False
-}
dentroMapa :: Mapa                           -- ^ recebe o mapa do jogo
            -> Posicao                       -- ^ recebe a posição de um determinado bloco
                -> Bool                      -- ^ devolve um Booleano
dentroMapa mapa (x,y) = (x >= 0 && x <= fromIntegral (length (head mapa))) && (y >= 0 && y <= fromIntegral (length mapa))


{-|A função 'checkCaminho' é responsável por verificar para um dado mapa com um portal e uma base, se existe um caminho em terra que ligue o portal e a base.
No caso 

==__Exemplos de Aplicação:__
>>>checkCaminho mapa01 (0,0) (4,4) []
True

>>>checkCaminho mapa01 (0,0) (4,5) []
False
-}
checkCaminho :: Mapa                         -- ^ recebe o mapa do jogo
                -> Posicao                   -- ^ recebe a posição de um Portal
                    -> Posicao               -- ^ recebe a posição da base
                        -> [Posicao]         -- ^ recebe um acomulador inicialmente vazio que acomula os blocos já visitados
                                -> Bool      -- ^ devolve um Booleano
checkCaminho mapa portal base ac
    | portal == base && not (null ac) = True              -- verifica se a posição a ser verificada é a posição da base, verificando se não é a primeira verificação, isto é, se o portal não está em cima da base
    | otherwise = any (\terreno -> checkCaminho mapa terreno base (portal: ac)) terrenoValido
    where
        (x, y) = portal
        redor = [(x+rx, y+ry) | (rx, ry) <- [(-1,0), (1,0), (0,1), (0,-1)]]
        terrenoValido = filter (\pos -> dentroMapa mapa pos && emTerra pos mapa && notElem pos ac) redor



{-|A função 'portalSobrepoe' é responsável por verificar se um Portal sobrepõe a Base ou alguma torre

==__Exemplos de Aplicação:__
>>> portalSobrepoe (0,0) (5,4) [(2,2)]
False

>>> portalSobrepoe (5,4) (5,4) [(2,2)]
True
-}
portalSobrepoe :: Posicao                    -- ^ recebe o portal em questão
                        -> Posicao           -- ^ recebe a base do jogo
                            -> [Posicao]     -- ^ recebe as torres do jogo
                                    -> Bool  -- ^ devolve um Booleano
portalSobrepoe (x,y) (bx,by) torres = (x,y) `elem` torres || (x,y) == (bx,by)

{-|A função 'umaOndaAtiva' é responsável por verificar se um dado portal tem no máximo uma onda átiva.

==__Exemplos de aplicação:__
>>>umaOndaAtiva (Portal (0,0) [(Onda [] 3 0 0), (Onda [inimigoEx] 5 5 0)])
True

-}
umaOndaAtiva :: Portal                       -- ^ recebe um Portal do jogo
                    -> Bool                  -- ^ devolve um Booleano
umaOndaAtiva Portal {ondasPortal=ondas} = length ondasAtivas <= 1
    where ondasAtivas = filter (\Onda {entradaOnda=entrada, inimigosOnda=inimigos} -> entrada == 0 && not (null inimigos)) ondas




-- Validação de Inimigos:


{-|A função 'validaInimigosAll' faz a validação dos inimigos de vários portais através da função 'validaInimigos'

==__Exemplos de Aplicação:__
>>> validaInimigosAll portalEx mapa01 torresEx [inimigoEx] 
True
-}
validaInimigosAll :: [Portal]                   -- ^  recebe os portais em jogo
                         -> Mapa                -- ^  recebe o mapa do jogo
                            -> [Torre]          -- ^  recebe as torres em jogo
                                -> [Inimigo]    -- ^  recebe os inimigos em jogo
                                        -> Bool -- ^  devolve um Booleano
validaInimigosAll portais mapa torres inEmJogo = all (\portal -> validaInimigos portal mapa torres inEmJogo) portais


{-|A função 'validaInimigos' é responsável por fazer todas a verificações, __relativas a portais__, necessárias
para que um jogo seja válido. Esta função é utilizada pela função 'validaJogo'.

==__Exemplos de Aplicação:__
>>> validaInimigos (Portal (0,0) [Onda [Inimigo Normal (0,0) Sul 10 0.5 10 10 [] Walk 1 0 [] Nothing] 3 0 0]) mapa01 torresEx [inimigoEx]
True
-}
validaInimigos :: Portal                        -- ^  recebe um portal em jogo
                    -> Mapa                     -- ^  recebe o mapa do jogo
                        -> [Torre]              -- ^  recebe as torres em jogo
                            -> [Inimigo]        -- ^  recebe os inimigos spawnados
                                    ->  Bool    -- ^  devolve um Booleano
validaInimigos Portal {posicaoPortal=posPortal ,ondasPortal=ondas} mapa torres inEmJogo =  inimigoNoPortal posPortal posInOndas                           -- verifica se os inimigos têm a posição do respetivo portal 
                                                                                        && all inimigoStatsOk inOndas && checkVel inAll                   -- verifica se as Stats dos inimigos não ativos são válidas, e se a velocidade de todos os inimigos é não negativa
                                                                                        && all (\(x,y) -> emTerra (x,y) mapa) posAll                      -- verifica se todos os inimigos estão posicionados sobre Terra
                                                                                        && not (all (\(x,y) -> inimigoSobrepoe (x,y) torres) posAll)      -- verifica se os inimigos sobrepoeem alguma torre
    where inOndas = map inimigosOnda ondas                                  -- extrai para uma lista os inimigos de todas as ondas (inimigos não ativos)
          posInOndas  =  map posicaoInimigo (concat inOndas)                -- extrai para uma lista as posições dos inimigos das ondas (inimigos não ativos)
          posInJogo = map posicaoInimigo inEmJogo                           -- extrai para uma lista as posições dos inimigos em jogo (ativos)
          posAll = posInOndas ++ posInJogo                                  -- soma numa lista as posições de todos os inimigos
          inAll = concat inOndas ++ inEmJogo                                -- soma numa lista todos os inimigos 

{-|A função 'inimigoNoPortal' é responsável por verificar se um inimigo tem a posição do respetivo portal.

==__Exemplo de Aplicação:__
>>>inimigoNoPortal (0,0) [(0,0)]
True
-}
inimigoNoPortal :: Posicao                  -- ^  recebe a posição de um portal
                 -> [Posicao]               -- ^  recebe as posições dos respetivos inimigos
                        -> Bool             -- ^  devolve um Booleano
inimigoNoPortal (x,y) = all (\(ix,iy) -> (x,y) == (ix,iy))


{-|A função 'inimigoStatsOk' é responsável por verificar se as stats dos inimigos são válidas. 
Isto inclui: o nível de vida é positivo; a lista de projeteis ativos é vazia; a velocidade não é negativa.

==__Exemplos de Aplicação:__
>>> inimigoStatsOk [inimigoEx]
False

>>> inimigoStatsOk [(Inimigo Normal (0,0) Sul 10 0.5 10 10 [] Walk 1 0 [] Nothing)]
True
-}
inimigoStatsOk :: [Inimigo]                 -- ^  recebe os inimigos que ainda não spawnaram
                        -> Bool             -- ^  devolve um Booleano
inimigoStatsOk = all (\Inimigo {vidaInimigo=vida, projeteisInimigo=projeteis} -> vida > 0 && null projeteis)

{-|A função 'checkVel' é responsável por verificar se a velocidade de todos os inimigos é não negativa.
É utilizada pela função 'validaInimigos'.
-}
checkVel :: [Inimigo]                       -- ^  recebe a lista de todos os inimigos do jogo
                -> Bool                     -- ^  devolve um Booleano
checkVel = all (\Inimigo {velocidadeInimigo=velocidade} -> velocidade > 0)

{-|A função 'projeteisNorm' é responsável por verificar se a lista de projeteis de um inimigo já em jogo, está normalizada.
A função 'checkSinergias' é integrada como auxiliar

==__Exemplos de aplicação:__
>>> projeteisNorm [(Projetil Fogo (Finita 3)), (Projetil Gelo (Finita 3))]
False

>>> projeteisNorm [(Projetil Gelo (Finita 3)), (Projetil Gelo (Finita 3))]
False

-}
projeteisNorm :: [Projetil]                 -- ^  recebe a lista de projeteis de um inimigo já em jogo
                        -> Bool             -- ^  devolve um Booleano
projeteisNorm projeteis =  length projeteis == length (nubBy (\pr1 pr2 -> tipoProjetil pr1 == tipoProjetil pr2) projeteis)
                        && checkSinergias projeteis

{-|A função 'checkSinergias' é responsável por verificar as condições de compatibilidade do jogo.
Nomeadamente, verifica se existem, simultaneamente, projéteis do tipo Fogo e Resina, ou do Tipo Fogo e Gelo.

==__Exemplos de Aplicação:__
>>> checkSinergias [(Projetil Fogo (Finita 3)), (Projetil Gelo (Finita 3))]
False

>>> checkSinergias [(Projetil Resina (Finita 3)), (Projetil Gelo (Finita 3))]
True
-}
checkSinergias :: [Projetil]                -- ^  recebe a lista de projeteis de um inimigo já em jogo
                        -> Bool             -- ^  devolve um Booleano
checkSinergias projeteis
    | Fogo `elem` tiposProjeteis = not (Resina `elem` tiposProjeteis || Gelo `elem` tiposProjeteis)
    | otherwise = True
    where tiposProjeteis = map tipoProjetil projeteis


{-|A função 'inimigoSobrepoe' é responsável por verificar se um inimigo sobrepõe alguma Torre

==__Exemplos de Aplicação:__
>>> inimigoSobrepoe (2,2) torresEx
True
-}
inimigoSobrepoe :: Posicao                   -- ^ recebe a posição de um inimigo
                    -> [Torre]               -- ^ recebe as torres do jogo
                        -> Bool              -- ^ devolve um Booleano
inimigoSobrepoe (x,y) torres = (x,y) `elem` posTorres
    where posTorres  = map posicaoTorre torres





-- Validação de Torres:


{-|A função 'validaTorres' é responsável por  por fazer todas a verificações, __relativas às Torres__, necessárias
para que um jogo seja válido. Esta função é utilizada pela função 'validaJogo'.

==__Exemplos de Aplicação:__
>>> validaTorres torresEx mapa01
True
-}
validaTorres :: [Torre]                      -- ^ recebe a lista de Torres em jogo
                    -> Mapa                  -- ^ recebe o mapa do jogo
                        -> Bool              -- ^ devolve um Booleano
validaTorres torres mapa =  all (\(x,y) -> emRelva (x,y) mapa) posTorres                -- | verifica se a torra está posicionada sobre relva
                         && all (\Torre {alcanceTorre=alcance} -> alcance >0) torres    -- | verifica se o alcance é um valor positivo
                         && all (\Torre {rajadaTorre=rajada} -> rajada > 0) torres      -- | verifica se a rajada é um valor positivo
                         && all (\Torre {cicloTorre=ciclo} -> ciclo >= 0) torres        -- | verifica se o ciclo é um valor não negativo
                         && length posTorres == length nubPosT                          -- | verifica se não há torres sobrepostas

    where posTorres = map posicaoTorre torres           -- extrai as posições de todas as Torres para um Lista
          nubPosT   = nub posTorres                     -- elimina elementos duplicados da lista de posições das torres


{-|A função 'emRelva' é responável por verificar se um dado bloco de coordenada (x,y) é um terreno de Relva.

==__Exemplos de Aplicação:__
>>>emRelva (2,0) mapa01
True
-}
emRelva :: Posicao                           -- ^  recebe a posição de um determinado bloco
             -> Mapa                         -- ^  recebe o mapa do jogo
                -> Bool                      -- ^  devolve um Booleano
emRelva (x,y) mapa = (mapa !! floor y) !! floor x == Relva




-- Validação da Base:

{-|A função 'validaBase' é responsável por fazer todas a verificações, __relativas à base__, necessárias
para que um jogo seja válido. Esta função é utilizada pela função 'validaJogo'.

==__Exemplos de Aplicação:__
>>> validaBase baseEx mapa01 portalEx torresEx
True
-}
validaBase :: Base                           -- ^ recebe a base do jogo
                -> Mapa                      -- ^ recebe o mapa do jogo
                    -> [Portal]              -- ^ recebe os portais do jogo
                        -> [Torre]           -- ^ recebe as torres do jogo
                                -> Bool      -- ^ devolve um Booleano
validaBase base@Base {posicaoBase=(x,y), creditosBase=creditos} mapa portais torres =  emTerra (x,y) mapa                       -- |verifica se a base está sobre Terra
                                                                                    && creditos >= 0                            -- |verifica se os créditos não são negativos
                                                                                    && not (baseSobrepoe base portais torres)   -- |verifica se a base não sobrepoe Portais ou Torres


{-|A função 'baseSobrepoe' é responsável por verificar se a Base sobrepõe algum portal ou alguma torre

==__Exemplos de Aplicação:__
>>> baseSobrepoe baseEx portalEx torresEx
False

-}
baseSobrepoe :: Base                         -- ^ recebe a base do jogo
            -> [Portal]                      -- ^ recebe os portais do jogo
                -> [Torre]                   -- ^ recebe as torres do jogo
                        -> Bool              -- ^ devolve um Booleano
baseSobrepoe Base {posicaoBase=(x,y)} portais torres = (x,y) `elem` posPortais || (x,y) `elem` posTorres
    where posPortais = map posicaoPortal portais
          posTorres  = map posicaoTorre torres



-- Funções de Debuging


{-| Função para testes-}
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


{-| Função para testes-}
jogoEx :: Jogo
jogoEx = Jogo baseEx portalEx torresEx mapa01 [inimigoEx] lojaEx

{-| Função para testes-}
lojaEx :: Loja
lojaEx = [(100, Torre (0,2) 10 5 1 3 3 (Projetil Gelo (Finita 3)))]

{-| Função para testes-}
inimigoEx :: Inimigo
inimigoEx = Inimigo Normal (1,1) Sul 10 0.5 10 10 [Projetil Fogo (Finita 3)] Walk 0 0 [] Nothing

{-| Função para testes-}
baseEx :: Base
baseEx = Base 100 (5,4) 0

{-| Função para testes-}
torresEx :: [Torre]
torresEx = [ Torre (2,2) 10 5 1 3 3 (Projetil Fogo (Finita 3)) ]

{-| Função para testes-}
portalEx :: [Portal]
portalEx = [ Portal (0,0) ondaEx]

{-| Função para testes-}
ondaEx :: [Onda]
ondaEx = [Onda
                            [Inimigo Normal (0,0) Sul 10 0.5 10 10 [] Walk 0 0 [] Nothing]
                                                                3 0 0]
