{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : Gonçalo José Arantes Fernandes <a111855@alunos.uminho.pt>
              Felipe Youssef Braga da Silva <a109385@alunos.uminho.pt>
Maintainer  : a111855@alunos.uminho.pt, a109385@alunos.uminho.pt


Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
-}
module Tarefa3 where

import LI12425
import Tarefa2
import Tarefa1 (emTerra)


import Debug.Trace

{-| 
A função 'atualizaJogo' é responsável por realizar todas as atualizações necessárias do jogo,
com o decorrer do tempo. São Integradas diversas funções auxiliares que ajudam no processo,
como a movimentação dos inimigos, a ativação dos portais, o ataque das torres e o processo de
atualização da base. As atualizações são feitas com base no 'Tempo' decorrido.

= _Processo geral:_
1. Atualiza os portais ativos e gera novos inimigos a partir deles.
2. Atualiza as torres, considerando os inimigos atingidos e os danos aplicados junto dos seus efeitos.
3. Atualiza a movimentação dos inimigos no mapa, considerando as modificações dos inimigos após 
as interações com as torres.
4. Processa os inimigos restantes, removendo os que foram derrotados e atualizando a base de acordo 
com os inimigos que ainda estão no jogo.
5. Atualiza os portais ativos após a passagem de tempo.

Essa função garante que todas as dinâmicas do jogo estejam em constante atualização conforme o tempo 
avança, refletindo as interações entre inimigos, torres, portais e a base do jogador.
-}
atualizaJogo :: Tempo                       -- ^ Recebe o 'Tempo' decorrido do 'Jogo'
                 -> Jogo                    -- ^ Recebe o próprio "Jogo"
                     -> Jogo                -- ^ Devolve o 'Jogo' com os seus valores atualizados
atualizaJogo tempo j@Jogo {baseJogo=base,
                           portaisJogo=portais,
                           torresJogo=torres,
                           mapaJogo=mapa,
                           inimigosJogo=inimigos} =
    let

        (portaisAtv, novosInimigos) = unzip (map ativaPortal portais)
        inimigosUptd = inimigos ++ concat novosInimigos                                             -- junto os inimigos já em jogo, aos novos inimigos lançados pelos portais

        (torresUpdtd, inimigosAposTorres) = atualizaTorres tempo torres inimigosUptd                --atualização das torres e inimigos atingidos por essas torres(inimigosAposTorres)

        inimigosAtualizados = atualizaInimigos mapa (posicaoBase base) tempo inimigosAposTorres     --A lista de inimigos atingidos por torres(inimigosAposTorres) é usada para a movimentaInimigos para mover apenas os atualizados!

        (inimigosRestantes, baseAtualizada) = processaInimigos inimigosAtualizados base

        portaisUpdtd = map (atualizaPortal tempo) portaisAtv                                        -- atualizo os portais que já passaram pela ativação


    in  j {baseJogo=baseAtualizada,
           portaisJogo=portaisUpdtd,
           torresJogo=torresUpdtd,
           mapaJogo=mapa,
           inimigosJogo=inimigosRestantes
    }


{-| 
A função 'atualizaTorres' é responsável por configurar o comportamento das torres em relação aos inimigos 
em um intervalo de tempo determinado; a cada iteração, ela verifica os inimigos no alcance de cada torre da lista,
dispara contra eles e atualiza os estados das torres e dos inimigos com base no tempo do gloss.

== __Funções Auxiliares:__
- 'inimigosNoAlcance':
    Filtra os inimigos que estão dentro do alcance de uma torre específica.

- 'disparaTorre':
    Aplica os danos da torre nos inimigos que estão dentro de seu alcance e atualiza o estado da torre.

== __Propriedades:__
1. Caso Base:
    @
    atualizaTorres _ [] inimigos = ([], inimigos)
    @
    - Quando não há torres para atualizar, a função retorna uma lista vazia de torres e a lista original de inimigos.

2. Caso Recursivo:
    @
    atualizaTorres tempo (torre:restoTorres) inimigos
    @
    - Filtra os inimigos no alcance da primeira torre usando 'inimigosNoAlcance'.
    - Atualiza a torre e os inimigos atingidos usando 'disparaTorre'.
    - Aplica recursivamente a função para o restante das torres e os inimigos atualizados.
-}
atualizaTorres :: Tempo                                 -- ^ Tempo decorrido desde a última atualização das torres
                    -> [Torre]                          -- ^ Lista de torres presentes no jogo
                        -> [Inimigo]                    -- ^ Lista de inimigos presentes no jogo
                            -> ([Torre], [Inimigo])     -- ^ Retorna a lista de torres atualizadas e a lista de inimigos atualizada.
atualizaTorres _ [] inimigos = ([], inimigos)
atualizaTorres tempo (torre:restoTorres) inimigos =
    let inimigosNoAlcance' = inimigosNoAlcance torre inimigos
        (novaTorre, inimigosAtualizados) = disparaTorre tempo torre inimigosNoAlcance'
        (torresRestantes, inimigosFinais) = atualizaTorres tempo restoTorres inimigos

        -- Combina os inimigos atualizados com os que não estavam no alcance
        inimigosResultantes = if null inimigosNoAlcance'
                             then inimigosFinais
                             else let posAtualizados = map posicaoInimigo inimigosAtualizados
                                      framesAtualizados = map frameInimigo inimigosAtualizados

                                      -- método para evitar sobreposição de inimigos com mesma posição. utilizamos frames como um id para evitar bug!!!
                                  in map (\i -> if posicaoInimigo i `elem` posAtualizados && frameInimigo i `elem` framesAtualizados
                                               then head (filter (\ia -> posicaoInimigo ia == posicaoInimigo i && frameInimigo ia == frameInimigo i) inimigosAtualizados)
                                               else i) inimigosFinais
    in  (novaTorre : torresRestantes, inimigosResultantes)



{-| 
A função 'disparaTorre' é responsável por configurar o estado de uma torre e aplica seus efeitos nos inimigos 
dentro do seu alcance. Ela considera o tempo decorrido para reduzir o tempo de recarga 
de UMA torre e aplica dano e efeitos nos inimigos atingidos. Ela, resumidamente, configura as mecânicas
de uma torre, para que a atualizaTorres "execute" para todas as torres.

= __Processo geral:__
1. Verifica se a torre está em recarga:
    - Se estiver, apenas atualiza o tempo de recarga restante.
    - Caso contrário, passa para o próximo passo.
2. Identifica os inimigos que estão no alcance da torre.
3. Seleciona até um número máximo de inimigos a serem atingidos, com base no parâmetro 'rajadaTorre'.
4. Aplica os danos e efeitos da torre aos inimigos selecionados.
5. Atualiza a lista de inimigos, substituindo os atingidos por suas versões atualizadas.
6. Reinicia o tempo de recarga da torre após o ataque.

-}
disparaTorre :: Tempo                            -- ^ Tempo decorrido desde a última atualização da torre     
                -> Torre                         -- ^ A torre que está realizando o ataque
                    -> [Inimigo]                 -- ^ Lista de inimigos presentes no jogo
                        -> (Torre, [Inimigo])    -- ^ Retorna a torre atualizada e a lista de inimigos após o ataque
disparaTorre tempo torre inimigos
  | null inimigos = (torre { tempoTorre = max 0 (tempoTorre torre - tempo) }, inimigos)
  | tempoTorre torre > 0 =
      trace ("Torre cooldown:" ++ show (tempoTorre torre))
      (torre { tempoTorre = max 0 (tempoTorre torre - tempo) }, inimigos)
  | otherwise =
      let
        -- Filtra os inimigos que estão no alcance da torre
        inimigosNoRaio = filter (\i -> distancia (posicaoTorre torre) (posicaoInimigo i) <= alcanceTorre torre) inimigos

        -- Seleciona até `rajadaTorre` inimigos para serem atingidos
        inimigosAlvo = take (rajadaTorre torre) inimigosNoRaio

        -- Atualiza os inimigos atingidos
        inimigosAtingidos = map (atingeInimigo torre) inimigosAlvo

        -- Substitui os inimigos alvo pelos inimigos atingidos na lista original
        todosInimigosAtualizados = map (\i ->
            -- método para evitar sobreposição de inimigos com mesma posição. utilizamos frames como um id para evitar bug!!!
          if posicaoInimigo i `elem` map posicaoInimigo inimigosAlvo && frameInimigo i `elem` map frameInimigo inimigosAlvo
          then head (filter (\ia -> posicaoInimigo ia == posicaoInimigo i && frameInimigo ia == frameInimigo i) inimigosAtingidos)
          else i) inimigos

        -- Atualiza o tempo de recarga da torre
        novaTorre = torre { tempoTorre = cicloTorre torre }
      in
        (novaTorre, todosInimigosAtualizados)                                 -- Tupla com a torre após CD e inimigos após disparo




--3.3.2

{-| 
A função "atualizaInimigos" é responsável por configurar a atualização completa em todos os inimigos no mapa, considerando os efeitos 
ativos de projéteis que podem alterar sua velocidade e vida, e ajustando suas posições conforme a direção e o terreno.

== Processo geral:

1. A função "atualizaInimigos" aplica a função auxiliar "atualizaInimigo" a cada inimigo da lista usando a função'map'.

2. "atualizaInimigo" processa um único inimigo, ajustando sua posição e projetando-o da seguinte forma:
    - Atualiza os projéteis usando atualizaDuracaoProjeteis,
    - Configura o efeito de fogo usando danoFogo,
    - Calcula a velocidade ajustada baseada nos efeitos dos projéteis(velocidadeAjustada),
    - Determina a nova posição considerando a direção e velocidade ajustada,
    - Verifica se a nova posição é válida no mapa:
        - Se válida: Atualiza a posição do inimigo,
        - Se inválida: Altera sua direção com "mudaDirecao".

== __Funções Auxiliares:__

- "atualizaInimigo":
    Atualiza a posição e os efeitos ativos de UM único inimigo, garantindo que a posição seja válida no mapa.

- "atualizaDuracaoProjeteis":
    Atualiza a duração dos projéteis aplicados ao inimigo, removendo efeitos expirados.

- "AplicarDanoFogo":
    Responsável pela configuração do dano por segundo do elemento fogo.

- "ajustaVelocidade":
    Calcula a velocidade ajustada do inimigo, considerando a velocidade base e os efeitos dos projéteis.

- "posicaoValida":
    Verifica se a nova posição do inimigo é válida no mapa (por exemplo, não é um terreno de água).

- "mudaDirecao":
    Altera a direção do inimigo caso a posição calculada seja inválida.
-}
atualizaInimigos :: Mapa                               -- ^ Recebe o 'Mapa' do 'Jogo', utilizado para validar as novas posições dos inimigos(AKA Terreno)
                    -> Posicao
                        -> Tempo                            -- ^ Recebe o tempo decorrido de 'Jogo'
                            -> [Inimigo]                    -- ^ Recebe a lista de 'Inimigo's em 'Jogo'
                                -> [Inimigo]                -- ^ Devolve a lista de Inimigos movimentados
atualizaInimigos mapa posicaoBase tempo inimigos = map (atualizaInimigo mapa posicaoBase tempo) inimigos     --map recebe a funcao e a lista



{-|
A função 'atualizaInimigo' é responsável por calcular o novo estado de um inimigo após a passagem de um intervalo de tempo. 
Esta atualiza a posição, a vida, os efeitos dos projéteis, e o trajeto do inimigo em direção à base.

= Processo geral:
1. Atualiza os projéteis que estão a afetar o inimigo, reduzindo as suas durações.
2. Calcula o dano causado pelos projéteis (como efeitos de fogo) e ajusta a vida do inimigo.
3. Ajusta a velocidade do inimigo com base nos projéteis que o atingem.
4. Verifica se o inimigo já tem um caminho definido:
    - Se sim, segue o caminho restante.
    - Se não, calcula um novo caminho em direção à base, caso possível.
5. Atualiza a posição do inimigo com base na sua velocidade e no tempo decorrido:
    - Se o progresso em direção ao próximo ponto do caminho for maior ou igual a 1, passa para o próximo ponto.
    - Caso contrário, calcula uma posição intermediária entre os pontos.
6. Atualiza a direção e o estado de animação do inimigo com base no movimento real.

Essa função garante que os inimigos sigam suas rotas no mapa e sejam afetados pelos elementos do jogo, como projéteis e obstáculos.
-}
atualizaInimigo :: Mapa             -- ^ Recebe o mapa do jogo
                -> Posicao          -- ^ Recebe a posição da base no mapa
                -> Tempo            -- ^ Recebe o tempo decorrido
                -> Inimigo          -- ^ Recebe o inimigo a ser atualizado
                -> Inimigo          -- ^ Devolve o inimigo atualizado
atualizaInimigo mapa posicaoBase tempo inimigo@Inimigo {
    posicaoInimigo = pos,
    direcaoInimigo = dir,
    velocidadeInimigo = v,
    projeteisInimigo = projeteis,
    vidaInimigo = vida,
    caminhoInimigo = caminho,
    posMedia = posMedia'
} =
    let
        -- Atualiza projéteis e vida
        projeteisNormalizados = foldr atualizaProjeteis [] projeteis  -- garante que estão normalizados
        
        projeteisAtualizados = trace ("Inimigo na pos " ++ show pos ++ " tem projeteis: " ++ show projeteisNormalizados) $
                               atualizaDuracaoProjeteis tempo projeteisNormalizados
        
        danoFogo = aplicarDanoFogo tempo projeteisAtualizados

        novaVida = max 0 (vida - danoFogo)
        velocidadeAjustada = ajustaVelocidade v projeteisAtualizados

        -- Atualiza ou obtém novo caminho
        novoCaminho = case caminho of
            [] -> case encontraCaminho mapa pos posicaoBase of      -- se não existir caminho, procura novo caminho
                    Just path -> tail path                          -- Remove a posição atual do inimigo
                    Nothing -> []                                   -- não há caminho
            _  -> caminho

        -- Atualiza posicao média e calcula nova posição
        (novaPosicao, novaPosMedia, caminhoRestante) =
            case (posMedia', novoCaminho) of
                -- Sem posicao média atual e tem caminho: inicia nova posicao média
                (Nothing, proxPonto:_) ->
                    let novaPos = movePosicao pos proxPonto (velocidadeAjustada * tempo)
                        distAtual = distancia novaPos proxPonto
                        progresso = 1 - (distAtual / distancia pos proxPonto)
                    in (novaPos, Just (pos, proxPonto, progresso), novoCaminho)

                -- Com posicao média ativa: continua ou atualiza
                (Just (orig, dest, prog), _) ->
                    let novoProgresso = prog + (velocidadeAjustada * tempo / distancia orig dest)
                    in if novoProgresso >= 1
                       then case novoCaminho of
                            -- Próximo ponto disponível: inicia nova posicao média
                            (proxPonto:resto) ->
                                (dest, Just (dest, proxPonto, 0), resto)
                            -- Fim do caminho: mantém posição final
                            [] -> (dest, Nothing, [])
                       else
                           -- Continua posicao média atual
                           let novaPos = calcPosIntermedia orig dest novoProgresso
                           in (novaPos, Just (orig, dest, novoProgresso), novoCaminho)

                -- Sem posicao média e sem caminho: mantém posição
                (Nothing, []) -> (pos, Nothing, [])

        -- Calcula nova direção baseada no movimento real
        novaDirecao = if novaPosicao == pos
                      then dir
                      else calculaDirecao pos novaPosicao

        -- Atualiza animação baseada no movimento
        novaAnimacao = if any (\p -> tipoProjetil p==Gelo) projeteisAtualizados
                       then Frozen
                       else Walk

    in inimigo {
        posicaoInimigo = novaPosicao,
        direcaoInimigo = novaDirecao,
        projeteisInimigo = projeteisAtualizados,
        vidaInimigo = novaVida,
        animacao = novaAnimacao,
        caminhoInimigo = caminhoRestante,
        posMedia = novaPosMedia
    }


{-|
A função 'encontraCaminho' gera o caminho em Terra mais curto, que ligue um Portal e a Base. 

= __Processo geral:__
1. Inicia a busca a partir da posição inicial, marcando-a como visitada e registando-a no caminho.
2. Em cada tile, verifica os vizinhos válidos da posição atual:
    - Um vizinho é válido se está dentro dos limites do mapa, é acessível (em terra) e se ainda não foi visitado.
3. Adiciona os vizinhos válidos à lista de posições a serem processadas.
4. Quando o Portal é atingido, retorna o caminho percorrido até ele.
5. Se não há caminho possível, retorna 'Nothing'.

Esta função é usada para guiar os inimigos no jogo, permitindo que eles sigam o trajeto até a base.
-}
encontraCaminho :: Mapa                             -- ^ Recebe o mapa do Jogo
                    -> Posicao                      -- ^ Recebe a posição inicial (inicialmente o portal)
                        -> Posicao                  -- ^ Recebe a posição final (Base do jogador)
                            -> Maybe [Posicao]      -- ^ Devolve, se existir, uma lista de posições a seguir
encontraCaminho mapa inicio fim = aux [(inicio, [inicio])] []
  where
    aux :: [(Posicao, [Posicao])]                   -- recebe a posicao atual e o caminho percorrido até ao momento
                        -> [Posicao]                -- armazena a lista de posições já visitadas
                                -> Maybe [Posicao]
    aux [] _ = Nothing                              -- não existe caminho
    aux ((pos, caminho):resto) visitados
      | pos == fim = Just (reverse caminho)         -- chegou à Base
      | pos `elem` visitados = aux resto visitados  -- posicao já visitada
      | otherwise = aux (resto ++ novosVizinhos) (pos:visitados) -- define pos como visitada e adiciona os vizinhos válidos à lista a ser verificada
      where
        (x, y) = pos
        vizinhos = [(x+dx, y+dy) | (dx, dy) <- [(0,1), (1,0), (0,-1), (-1,0)]]
        vizinhosValidos = filter (\p -> posicaoValida mapa p && emTerra p mapa) vizinhos
        novosVizinhos = [(v, v:caminho) | v <- vizinhosValidos, v `notElem` visitados]


{-|
A função 'calcPosIntermedia' calcula uma posição intermediária entre duas posições, 
baseando-se  num parâmetro de progresso t. O valor de t deve estar no intervalo [0,1], 
onde 0 corresponde à posição inicial e 1 à posição final.

Esta função é necessária para evitar saltos entre as movimentações dos inimigos, fazendo
com que seja mais suave.

== __Exemplo de Aplicação:__

>>>calcPosIntermedia (0, 0) (10, 10) 0.5
(5.0,5.0)
-}
calcPosIntermedia :: Posicao                        -- ^ Recebe uma posição inicial
                        -> Posicao                  -- ^ Recebe uma posição final
                                -> Float            -- ^ Recebe o progresso entre os dois pontos
                                    -> Posicao      -- ^ Devolve a posição intermédia
calcPosIntermedia (x1, y1) (x2, y2) t =
    let clampedT = max 0 (min 1 t)  -- Garante que t está entre 0 e 1
        newX = x1 + (x2 - x1) * clampedT
        newY = y1 + (y2 - y1) * clampedT
    in (newX, newY)


{-|
A função 'movePosicao' calcula a nova posição de um objeto em movimento, partindo de uma posição inicial
em direção a uma posição final, considerando uma distância máxima de movimento.

- Caso o objeto esteja muito próximo do destino (menos de 0.01 unidades), este move-se diretamente para o destino.
- Caso contrário, a função normaliza o vetor direção e aplica o movimento proporcional à distância.

== __Exemplo de Aplicação:__

>>>movePosicao (0, 0) (10, 0) 3
(3.0,0.0)
-}
movePosicao :: Posicao                          -- ^ Recebe a posição inicial
                -> Posicao                      -- ^ Recebe a posição final
                    -> Float                    -- ^ Recebe a distáncia máxima de movimento (depende da velocidade do inimigo)
                        -> Posicao              -- ^ Devolve a nova posição após o movimento
movePosicao (x1, y1) (x2, y2) dist =
    let dx = x2 - x1
        dy = y2 - y1
        len = sqrt (dx * dx + dy * dy)  
    in if len < 0.01  -- Se estiver muito próximo do destino
       then (x2, y2)  -- Vai direto para o destino
       else
           let normalizedDx = dx / len
               normalizedDy = dy / len
               movement = min dist len  -- Limita o movimento à distância até o destino
               newX = x1 + (normalizedDx * movement)
               newY = y1 + (normalizedDy * movement)
           in (newX, newY)


{-|
A função 'calculaDirecao' determina a direção entre duas posições, com base nas coordenadas suas coordenadas.

= __Processo geral:__
- Se a posição final está à direita da inicial, a direção é 'Este'.
- Se está à esquerda, a direção é 'Oeste'.
- Se está acima, a direção é 'Norte'.
- Caso contrário, a direção é 'Sul'.

-}
calculaDirecao :: Posicao                   -- ^ Recebe a posição inicial
                    -> Posicao              -- ^ Recebe a posição final
                        -> Direcao          -- ^ Devolve a nova direção
calculaDirecao (x1,y1) (x2,y2)
    | x2 > x1 = Este
    | x2 < x1 = Oeste
    | y2 > y1 = Norte
    | otherwise = Sul


{-|
A função 'distancia' calcula a distância entre dois pontos no plano.

== __Exemplo de Aplicação:__

>>>distancia (0, 0) (3, 4)
5.0
-}
distancia :: Posicao                        -- ^ Recebe um ponto
            -> Posicao                      -- ^ Recebe um segundo ponto
                -> Float                    -- ^ Devolve a distância entre eles
distancia (x1,y1) (x2,y2) =sqrt ((x2-x1)^(2::Int) + (y2-y1)^(2::Int))


{-|
A função 'atualizaDuracaoProjeteis' atualiza a duração dos projéteis no jogo com base no tempo decorrido. 
Projéteis com duração finita sofrei uma redução da sua duração e são removidos se esta atingir 0. 
Projéteis com duração infinita não são alterados.

= __Processo geral:__
1. Reduz a duração de projéteis com duração finita.
2. Remove projéteis cuja duração chegou a zero.

-}
atualizaDuracaoProjeteis :: Tempo                       -- ^ Recebe o tempo decorrido desde a última atualização
                            -> [Projetil]               -- ^ Recebe a lista de projeteis a ser atualizada
                                -> [Projetil]           -- ^ Devolve a lista de projeteis a ser atualizada
atualizaDuracaoProjeteis tempo projeteis = filter (not . duracaoExpirou) (map atualizaDuracao projeteis)
  where
    atualizaDuracao :: Projetil -> Projetil
    atualizaDuracao projetil@Projetil { duracaoProjetil = Finita t }
        | t > 0     = projetil { duracaoProjetil = Finita (t - tempo) } -- Reduz a duração
        | otherwise = projetil { duracaoProjetil = Finita 0 }          -- Expira o projétil
    atualizaDuracao projetil = projetil -- Projéteis com duração infinita permanecem inalterados
    
    duracaoExpirou :: Projetil -> Bool
    duracaoExpirou projetil = case duracaoProjetil projetil of
        Finita x -> x <= 0
        _        -> False


{-| A função 'ajustaVelocidade' é responsável por calcular a velocidade ajustada com relação aos impactos de uma lista de projéteis. 
Sempre levando em consideração modificadores específicos de cada projétil!

== __Configuração:__

1. **Modificadores de Velocidade:**
    @
    | modificador = calculaModificador projetil
    @
    - Para cada projétil, a função 'calculaModificador' é chamada para calcular o impacto específico sobre a velocidade.

2. **Soma dos Modificadores:**
    - Os modificadores de velocidade de todos os projéteis são somados para ajustar a velocidade final.

3. **Velocidade Final:**
    - A função garante que a velocidade não possa ser negativa, aplicando 'max 0' à soma dos impactos.
-}
ajustaVelocidade :: Float                           -- ^ Recebe a velocidade inicial do inimigo
                    -> [Projetil]                   -- ^ Recebe a lista de projeteis do inimigo
                            -> Float                -- ^ Devolve a velocidade ajustada
ajustaVelocidade velocidade projeteis =
    let
       -- Calcula os impactos na velocidade de todos os projeteis
        modificadores = map (calculaModificador velocidade) projeteis
        -- Garante que a velocidade não seja negativa
        velocidadeFinal = max 0 (velocidade - sum modificadores)
    in
        velocidadeFinal



{-| A função 'calculaModificador' é responsável por determinar o impacto de um projétil sobre a velocidade, 
baseado em seu tipo e tempo restante de sua duração.

== __Configuração:__

1. **Projétil do Tipo 'Resina':**
    - Se o tempo restante (t) for maior que zero, aplica uma redução de 0.6 à velocidade.
    - Caso contrário, não aplica impacto.

2. **Projétil do Tipo 'Gelo':**
    - Se o tempo restante (t) for maior que zero, aplica uma redução de 1 à velocidade.
    - Caso contrário, não aplica impacto.

3. **Outros Tipos de Projéteis:**
    - Não aplicam nenhum modificador à velocidade, retornando 0.
-}
calculaModificador :: Float                         -- ^ Recebe a velocidade atual do inimigo
                        -> Projetil                 -- ^ Recebe um projetil da lista de projeteis do inimigo
                                -> Float            -- ^ Devolve a redução de velocidade necessária
calculaModificador _ Projetil { tipoProjetil = Fogo } = 0.0
calculaModificador velocidade Projetil { tipoProjetil = Gelo,   duracaoProjetil = Finita t   }
    | t > 0     = velocidade        -- reduz a velocidade por completo
    | otherwise = 0.0
calculaModificador velocidade Projetil { tipoProjetil = Gelo,   duracaoProjetil = Infinita   } = velocidade
calculaModificador velocidade Projetil { tipoProjetil = Resina, duracaoProjetil = Finita t   }
    | t > 0     = velocidade * 0.50 -- reduz 50%
    | otherwise = 0.0
calculaModificador velocidade Projetil { tipoProjetil = Resina, duracaoProjetil = Infinita   } = velocidade * 0.50



{-| A função 'posicaoValida' é responsável por verificar se uma posição em um mapa é válida, ou seja, 
se o terreno na posição especificada não é de água ou relva!!

== __Configuração:__

1. **Verificação de Terreno:**
    - A função acessa o mapa na posição correspondente (x, y) e verifica o tipo de terreno. 
    - Se o terreno for 'Agua' ou 'Relva', a posição é considerada inválida.

2. **Acesso ao Mapa:**
    - A função utiliza o operador de índice '!!' para acessar o mapa e determinar o terreno na posição fornecida.
    - As coordenadas x e y são arredondadas para inteiros usando 'floor' antes de serem usadas como "índices".
-}
posicaoValida :: Mapa                           -- ^ Recebe o mapa do jogo
                 -> Posicao                     -- ^ Recebe uma posicao
                      -> Bool                   -- ^ Devolve um Booleano
posicaoValida mapa (x, y) = not (yIndex < 0 || yIndex >= length mapa || xIndex < 0 || xIndex >= length (mapa !! yIndex)) && (let terreno = (mapa !! floor y) !! floor x
                                                                                                                             in terreno /= Agua && terreno /= Relva)
    where xIndex = floor x
          yIndex = floor y


{-|
A função "processaInimigos" é responsável por processar a lista de inimigos, atualizando o estado da base e r
emovendo inimigos que tenham sido derrotados ou atingido a base.

== __Configuração:__

1. A função "processaInimigos" utiliza a função "foldl" para processar cada inimigo da lista, verificando as seguintes condições:
   - Se o inimigo foi derrotado (vida <= 0), ele é removido e o butim do inimigo é adicionado aos créditos da base.
   - Se o inimigo atingiu a base, a vida da base é reduzida pelo ataque do inimigo.
   - Caso contrário, o inimigo permanece na lista.

-}
processaInimigos :: [Inimigo]                       -- ^ Recebe a lista de inimigos a serem processados
                        -> Base                     -- ^ Recebe a base do jogo
                            -> ([Inimigo], Base)    -- ^ Devolve a lista de inimigos restates e a base atualizada
processaInimigos inimigos base =
    trace ("Processando inimigos. Total: " ++ show (length inimigos)) $
    foldr processa ([], base) inimigos
  where
    processa inimigo (inimigosRestantes, baseAtualizada) =
        trace ("Processando inimigo. Vida: " ++ show (vidaInimigo inimigo) ++
              " Posicao: " ++ show (posicaoInimigo inimigo)) $
        if vidaInimigo inimigo <= 0
        then trace "Inimigo eliminado por vida <= 0" $
             adicionaButim inimigo baseAtualizada inimigosRestantes
        else if atingiuBase inimigo baseAtualizada
        then trace "Inimigo atingiu a base" $
             aplicaDano inimigo baseAtualizada inimigosRestantes
        else (inimigo : inimigosRestantes, baseAtualizada)

    adicionaButim :: Inimigo -> Base -> [Inimigo] -> ([Inimigo], Base)
    adicionaButim inimigo baseJ inimigosJ =
        (inimigosJ, baseJ { creditosBase = creditosBase base + butimInimigo inimigo })

    aplicaDano :: Inimigo -> Base -> [Inimigo] -> ([Inimigo], Base)
    aplicaDano inimigo baseJ inimigosJ =
        (inimigosJ, baseJ { vidaBase = max 0 (vidaBase base - ataqueInimigo inimigo) })



-- | A função "atingiuBase" verifica se um inimigo atingiu a base, comparando suas posições.
atingiuBase :: Inimigo                          -- ^ Recebe o inimigo a ser verificado
                -> Base                         -- ^ Recebe a base do jogador
                    -> Bool                     -- ^ Devolve um Booleano
atingiuBase Inimigo { posicaoInimigo = (x1, y1) } Base { posicaoBase = (x2, y2) } =
    distancia (x1, y1) (x2, y2) <= tolerancia
  where
    tolerancia = 0.1  -- Define a margem de erro aceitável

{-|
A função "aplicarDanoFogo" aplica dano aos inimigos com base nos projéteis de fogo que atingem o inimigo, 
levando em consideração a duração desses projéteis.

== __Funcionamento:__

A função percorre a lista de projéteis e, caso o tipo do projétil seja "Fogo", aplica um dano de 10 por segundo 
multiplicado pelo tempo. Se o projétil não for de fogo, não causa dano.
-}
aplicarDanoFogo :: Tempo                        -- ^ Recebe o tempo decorrido desde a última atualização
                    -> [Projetil]               -- ^ Recebe a lista de projeteis atualizados
                            -> Float            -- ^ Devolve o dano a aplicar no devido frame
aplicarDanoFogo tempo projeteisAtualizados =
    let danoPorSegundo = 1  -- dano base por segundo
        danoNesteFrame = danoPorSegundo * tempo
    in if any (\p -> tipoProjetil p == Fogo) projeteisAtualizados
       then danoNesteFrame  -- aplica o dano 
       else 0  -- Não aplica dano





--3.3.3 
-- Comportamento dos portais


{-|
A função "atualizaPortal" é responsável por atualizar os temporizadores das ondas dos portais 
de acordo com o tempo decorrido no jogo. Ela integra a função "atualizaOnda", que atualiza 
os detalhes de cada onda individualmente.

== Funcionamento:
A função percorre as ondas do portal e para cada onda chama a função "atualizaOnda" 
para atualizar o seu temporizador com base no tempo decorrido.
-}
atualizaPortal :: Tempo                     -- ^ Recebe o 'Tempo' decorrido do 'Jogo'
                 -> Portal                  -- ^ Recebe um 'Portal' do 'Jogo'
                     -> Portal              -- ^ Devolve o 'Portal' com os respetivos valores atualizados
atualizaPortal tempo p@Portal {ondasPortal=ondas} = p {ondasPortal=novasOndas}
    where novasOndas = map (atualizaOnda tempo) ondas


{-|
A função "atualizaOnda" é responsável por atualizar o estado de uma onda de inimigos em um portal 
com base no tempo decorrido. Ela ajusta os temporizadores de ativação e lançamento dos inimigos 
de acordo com o tempo passado.

== Comportamento:
- Se a onda ainda não está ativa (entrada > 0), o tempo de entrada diminui.
- Se a onda já está ativa (entrada == 0) e o delay de lançamento dos inimigos ainda não expirou (delay > 0), o tempo do delay diminui.
- Se a onda está ativa e o delay expirou (delay == 0), o temporizador é reiniciado para o valor do ciclo de onda.


== Funcionamento:
A função realiza a atualização dos campos da "Onda" conforme a condição:
1. Se a onda ainda não foi ativada, o tempo de entrada diminui.
2. Se a onda está ativa, mas ainda há delay, o tempo de delay diminui.
3. Se o delay expirou, o temporizador é resetado para o ciclo da onda.
-}
atualizaOnda :: Tempo                       -- ^ Recebe o 'Tempo' decorrido do 'Jogo'
                 -> Onda                    -- ^ Recebe uma 'Onda' de um 'Portal'
                     -> Onda                -- ^ Devolve a 'Onda' com os respetivos valores atualizados
atualizaOnda tempo o@Onda {entradaOnda=entrada, tempoOnda=delay, cicloOnda=ciclo}
    | entrada > 0 = o {entradaOnda=entrada - tempo}         -- se a onda ainda não está ativa, então o tempo de ativação diminui
    | delay > 0 = o {tempoOnda=delay - tempo}               -- se a onda já está ativa (entrada == 0), então o delay entre lançamento de inimigos diminui
    | otherwise = o {tempoOnda = ciclo}                     -- se a onda está ativa e o delay de lançamento é 0, então resetamos o timer do delay para o cicloOnda


{-|
A função "ativaPortal" é responsável por ativar um portal e lançar inimigos caso haja alguma onda ativa.
Ela verifica se o portal possui ondas ativas e, se for o caso, chama a função 'ativaInimigo' para gerar 
os inimigos correspondentes.

== Comportamento:
- Se o portal tiver uma onda ativa, a função chama "ativaInimigo" para gerar os inimigos e atualiza o portal.
- Se o portal não tiver ondas ativas, ele permanece no mesmo estado e não lança inimigos.

== Funcionamento:
A função verifica as ondas ativas no portal:
- Se houver uma onda ativa (com "entradaOnda" maior que zero), a função chama "ativaInimigo" para lançar inimigos e atualiza o portal.
- Se não houver ondas ativas, o portal permanece inalterado e não há inimigos gerados.

-}
ativaPortal :: Portal                       -- ^ Recebe um 'Portal' do 'Jogo'       
                 -> (Portal, [Inimigo])     -- ^ Devolve um par com o respetivo 'Portal' atualizada, e a lista de 'Inimigo' lançados por ele
ativaPortal portal = if any shouldSpawn ondas then ativaInimigo portal []   -- se o portal tem uma onda ativa (primeira onda da lista), então o portal vai ativarInimigo
                                              else (portal,[])              -- se o portal não tem ondas ativas, então mantêm-se o mesmo, e não lança inimigos
    where ondas = ondasPortal portal


{-|
A função "shouldSpawn" verifica se uma onda deve ou não lançar um inimigo. A condição para o lançamento de um inimigo é que a onda esteja ativa e que o 'tempoOnda' (tempo de delay) seja igual a 0.

== Comportamento:
- A onda é considerada ativa quando "entradaOnda" é 0, o que significa que a onda já foi ativada.
- O lançamento de inimigos é condicionado a o tempo de delay ("tempoOnda") ser igual a 0, ou seja, não há mais atraso para o lançamento.

== Funcionamento:
A função retorna "True" se a onda estiver ativa e sem delay, ou seja, pronta para lançar um inimigo. Caso contrário, retorna "False".
-}
shouldSpawn :: Onda                         -- ^ Recebe uma 'Onda' de um 'Portal'
                 -> Bool                    -- ^ Devolve um Booleano                 
shouldSpawn Onda {entradaOnda=entrada, tempoOnda=delay} = entrada <= 0 && delay <= 0
