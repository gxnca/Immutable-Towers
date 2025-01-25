{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : (c) Gonçalo José Arantes Fernandes <a111855@alunos.uminho.pt> | Felipe Youssef Braga da Silva <a109385@alunos.uminho.pt>
Maintainer  : a109385@alunos.uminho.pt

Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}
module Tarefa2 where

import LI12425

import Debug.Trace

{-| A funcao 'inimigosNoAlcance' tem como objetivo relacionar a posicao da torre e o seu alcance para definir
se um inimigo qualquer está no alcance dessa torre.

== __Exemplos de Aplicação:__
>>> inimigosNoAlcance (Torre {posicaoTorre=(2,4), alcanceTorre= 10.2, danoTorre=2.0, rajadaTorre=2, cicloTorre=1.0, tempoTorre=1.0, projetilTorre= Projetil Fogo (Finita 3)}) [Inimigo Normal (3,6) Sul 10.0 1.0 5.0 10 [] Walk 1 0 [] Nothing]
[Inimigo {tipoInimigo = Normal, posicaoInimigo = (3.0,6.0), direcaoInimigo = Sul, vidaInimigo = 10.0, velocidadeInimigo = 1.0, ataqueInimigo = 5.0, butimInimigo = 10, projeteisInimigo = [], animacao = Walk, frameInimigo = 1, tempoAInimigo = 0.0, caminhoInimigo = [], posMedia = Nothing}]

>>> inimigosNoAlcance (Torre {posicaoTorre = (2, 4), alcanceTorre = 10.2, danoTorre=2.0, rajadaTorre=2, cicloTorre=1.0, tempoTorre=1.0, projetilTorre= Projetil Fogo (Finita 3)}) [Inimigo Normal (20,20) Sul 10.0 1.0 5.0 10 [] Walk 1 0 [] Nothing]
[]

== __Propriedades:__
- Para cada inimigo na lista de output(dentro do range da torre), a distancia entre sua posiçao e a posicao da torre deve ser menor ou igual ao alcance da torre:
    @
    all (\Inimigo {posicaoInimigo = (xi, yi)} -> sqrt ((xi - xt)^2 + (yi - yt)^2) <= alcance) 
        (inimigosNoAlcance (Torre {posicaoTorre = (xt, yt), alcanceTorre = alcance}) inimigos)
    @

- Para cada inimigo na lista de output(fora do range da torre), a distancia entre a sua posicao e a posicao da torre precisa ser maior que o alcance da torre,logo ele nao estara no alcance(range) da torre e, portanto, fora da lista de output: 
    @
    all (\Inimigo {posicaoInimigo = (xi, yi)} -> sqrt ((xi - xt)^2 + (yi - yt)^2) > alcance) 
        (inimigos \\ inimigosNoAlcance (Torre {posicaoTorre = (xt, yt), alcanceTorre = alcance}) inimigos)
    @

== __Função Auxiliar:__
- A funcao "estaNoAlcance" simplifica o calculo da distancia entre dois pontos(posicaoInimigo e posicaoTorre) para ser utilizada pelo filter

    @
    estaNoAlcance Inimigo {posicaoInimigo=(xi, yi)} = 
        sqrt ((xi-xt)^(2::Int)+(yi-yt)^(2::Int))  <= alcance
    @
-}
inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance Torre {posicaoTorre=(xt,yt), alcanceTorre=alcance} = filter estaNoAlcance
    where
        estaNoAlcance Inimigo {posicaoInimigo=(xi, yi)} = sqrt ((xi-xt)^(2::Int) + (yi-yt)^(2::Int)) <= alcance



{-| A funcao 'atingeInimigo' aplica o dano de uma torre a um inimigo específico, reduzindo sua vida 
com base no dano da torre e atualizando a lista de projéteis ativos no inimigo com o respectivo projétil da torre.

== __Exemplos de Aplicação:__
>>> atingeInimigo (Torre {danoTorre = 5, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil=(Finita 3)}, posicaoTorre = (0, 0), alcanceTorre = 10, rajadaTorre=2, cicloTorre=2.0, tempoTorre=2.0}) (Inimigo {tipoInimigo = Normal, posicaoInimigo = (3.0,6.0), direcaoInimigo = Sul, vidaInimigo = 10.0, velocidadeInimigo = 1.0, ataqueInimigo = 5.0, butimInimigo = 10, projeteisInimigo = [], animacao = Walk, frameInimigo = 1, tempoAInimigo = 0.0, caminhoInimigo = [], posMedia = Nothing})
Inimigo {tipoInimigo = Normal, posicaoInimigo = (3.0,6.0), direcaoInimigo = Sul, vidaInimigo = 5.0, velocidadeInimigo = 1.0, ataqueInimigo = 5.0, butimInimigo = 10, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}], animacao = Walk, frameInimigo = 1, tempoAInimigo = 0.0, caminhoInimigo = [], posMedia = Nothing}

>>> atingeInimigo (Torre {danoTorre = 15, projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil=(Finita 3)}, rajadaTorre=1, cicloTorre=2.0, tempoTorre=1.0, posicaoTorre = (0, 0), alcanceTorre = 10}) (Inimigo {tipoInimigo = Normal, posicaoInimigo = (3.0,6.0), direcaoInimigo = Sul, vidaInimigo = 10.0, velocidadeInimigo = 1.0, ataqueInimigo = 5.0, butimInimigo = 10, projeteisInimigo = [], animacao = Walk, frameInimigo = 1, tempoAInimigo = 0.0, caminhoInimigo = [], posMedia = Nothing})
Inimigo {tipoInimigo = Normal, posicaoInimigo = (3.0,6.0), direcaoInimigo = Sul, vidaInimigo = 0.0, velocidadeInimigo = 1.0, ataqueInimigo = 5.0, butimInimigo = 10, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}], animacao = Walk, frameInimigo = 1, tempoAInimigo = 0.0, caminhoInimigo = [], posMedia = Nothing}

== __Propriedades:__
- A vida atualizada do inimigo nunca será negativa:
    @
    novaVida = max 0 (vida - dano)
    @

== __Funcoes Auxiliares:__
* Função 'atualizaProjeteis'
* Função 'combinaComExistentes'
-}
atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo Torre {danoTorre=dano, projetilTorre=projetilImpacto} inimigo@Inimigo {vidaInimigo=vida, projeteisInimigo=projeteisAtivos} = inimigo {vidaInimigo = novaVida, projeteisInimigo = novaListaProjeteis}
  where
    novaVida = max 0 (vida - dano)                                                            -- evita valores negativos na nova vida depois de ser atingido
    novaListaProjeteis = atualizaProjeteis projetilImpacto projeteisAtivos                    {- Atualiza a "projeteisInimigo" com a novaListaProjeteis, decomposta em 3 variáveis (projetilImpacto da Torre, atualizaProjeteis 
                                                                                                que é responsável por atualizar a lista dos projeteis e projeteisAtivos) possibilitando as regras de sinergias-}



{-| A função 'atualizaProjeteis' adiciona um novo projétil a uma lista de projéteis ativos de um inimigo.

== __Exemplos de Aplicação:__

>>> atualizaProjeteis (Projetil {tipoProjetil = Fogo, duracaoProjetil=(Finita 3)}) [Projetil {tipoProjetil = Gelo, duracaoProjetil=(Finita 3)}]
[]
>>> atualizaProjeteis (Projetil {tipoProjetil = Resina, duracaoProjetil=(Finita 3)}) []
[Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 3.0}]
-}
atualizaProjeteis :: Projetil -> [Projetil] -> [Projetil]
atualizaProjeteis impacto [] = [impacto]
atualizaProjeteis impacto@(Projetil tipoImpacto duracaoImpacto) projeteis = trace ("Combinando projétil " ++ show impacto ++ " com " ++ show projeteis) $
    case tipoImpacto of

        Fogo ->
            if any (\p -> tipoProjetil p == Gelo) projeteis then                              -- se o projetil anterior for igual ao gelo, retira o gelo e o fogo não é adicionado
                [p | p <- projeteis, tipoProjetil p /= Gelo]                                  -- cria uma lista de projeteis, removendo Gelo (o Projetil de fogo não é adicionado)
            else if any (\p -> tipoProjetil p == Resina) projeteis then
                let novaDuracao = case duracaoImpacto of
                        Finita d -> Finita (d * 2)                                            -- a duração do efeito de fogo vai ser dobrada se o precursor dele for resina
                        Infinita -> Infinita
                in Projetil Fogo novaDuracao : [p | p <- projeteis, tipoProjetil p /= Resina] -- Dobra a duração e remove Resina
            else combinaComExistentes impacto projeteis

        Gelo ->
            if any (\p -> tipoProjetil p == Fogo) projeteis then
                [p | p <- projeteis, tipoProjetil p /= Fogo]                                  -- cria uma lista de projeteis, removendo Fogo (o Projetil de Gelo também não é adicionado)
            else combinaComExistentes impacto projeteis

        Resina -> combinaComExistentes impacto projeteis



{-|A função 'combinaComExistentes' verifica se dois projéteis possuem o mesmo tipo e as suas possiveis Sinergias.

== __Exemplos de Aplicação:__

>>> combinaComExistentes (Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3}) [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 2}]
[Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}]

>>> combinaComExistentes (Projetil {tipoProjetil = Gelo, duracaoProjetil = Infinita}) [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3}]
[Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Infinita}]
-}
combinaComExistentes :: Projetil -> [Projetil] -> [Projetil]
combinaComExistentes projetil [] = [projetil]
combinaComExistentes (Projetil tipoImpacto duracaoImpacto) (p:ps)
    | tipoProjetil p == tipoImpacto =
        let novaDuracao = case (duracaoImpacto, duracaoProjetil p) of
                (Infinita, _) -> Infinita
                (_, Infinita) -> Infinita
                (Finita d1, Finita d2) -> Finita (d1 + d2)
        in Projetil tipoImpacto novaDuracao : ps
    | otherwise = p : combinaComExistentes (Projetil tipoImpacto duracaoImpacto) ps



{-| A função 'ativaInimigo' tem como objetivo receber um portal, ativando inimigos da onda atual 
para adicioná-los à lista de inimigos ativos. 
Se a onda atual do portal não possuir mais inimigos, a função avança para a próxima onda. 
Caso o portal não tenha ondas restantes, nenhum inimigo é ativado!!

== __Propriedades:__

- Para um portal sem ondas, a lista de inimigos ativos permanece inalterada
- Após a ativação, o portal nunca mantém a mesma lista de inimigos na onda atual, pois esses inimigos sao inseridos no mapa
-}
ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo portal inimigosAtivos = case ondasPortal portal of
    [] -> (portal, inimigosAtivos) -- Nenhuma onda para ativar
    (ondaAtual:restoOndas) -> case inimigosOnda ondaAtual of
        [] -> (portal { ondasPortal = restoOndas }, inimigosAtivos) -- Onda atual está vazia, passa para a próxima
        (inimigoAtual:restoInimigos) ->
            let novaOnda = ondaAtual { inimigosOnda = restoInimigos }
                novoPortal = portal { ondasPortal = novaOnda : restoOndas }
                novoInimigosAtivos = inimigoAtual : inimigosAtivos
            in (novoPortal, novoInimigosAtivos)



{-| A função 'terminouJogo' verifica se o jogo atingiu uma condição de término. 
O jogo termina se:

    1. O jogador perdeu, condição indicada pela função auxiliar 'perdeuJogo'.
    2. O jogador ganhou, condição indicada pela função auxiliar 'ganhouJogo'.

== __Propriedades:__

- Um jogo em que nem 'perdeuJogo' nem 'ganhouJogo' é verdadeiro deve retornar False
- Um jogo em que 'terminouJogo' é True implica que pelo menos uma das condições de término foi atingida, ou ganhou ou perdeu
-}
terminouJogo :: Jogo -> Bool
terminouJogo jogo = perdeuJogo jogo || ganhouJogo jogo      -- Para terminar, o jogador precisa ter ganhado ou perdido



{-| A função 'ganhouJogo' verifica se o jogador venceu o jogo. O jogo é considerado ganho se:
   
    1. A base do jogador tem vida superior a zero (indicado por 'vidaBase').
    2. Não há inimigos restantes no jogo (indicados por 'inimigosJogo' do jogo).
    3. Todas as ondas dos portais estão sem inimigos (verificado pela função auxiliar 'semInimigos').

== __Propriedades:__

- Um jogo onde a base tem vida > 0, não há inimigos e todas as ondas estão sem inimigos é considerado "ganho".

== __Funcao Auxiliar:__
* Função 'semInimigos' 
-}
ganhouJogo :: Jogo -> Bool
ganhouJogo jogo = vidaBase (baseJogo jogo) > 0 && null (inimigosJogo jogo) && all semInimigos (portaisJogo jogo) --Condição para ganhar o jogo, base com vida > 0 e sem inimigos restantes em jogo, bem como nas ondas



{-| A função 'semInimigos' verifica se um portal não possui inimigos em suas ondas. Um portal é considerado sem inimigos se todas as ondas dentro do portal não tiverem inimigos nelas.

-}
semInimigos :: Portal -> Bool
semInimigos portal = all null inimigosOndas
    where ondas = ondasPortal portal
          inimigosOndas = map inimigosOnda ondas



{-| A função 'perdeuJogo' verifica se o jogador perdeu o jogo. O jogo é considerado "perdido" se a vida da base do jogador for menor ou igual a zero.

== __Propriedades:__
- Um jogo é perdido quando a vida da base for menor ou igual a zero.
-}
perdeuJogo :: Jogo -> Bool
perdeuJogo jogo = vidaBase (baseJogo jogo) <= 0             -- Condição para perder o jogo (vida da base <= 0)

