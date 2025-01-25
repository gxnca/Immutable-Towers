module Tarefa2Spec (testesTarefa2) where

import Test.HUnit
import Tarefa2
import LI12425

torres :: [Torre]
torres = []         -- valor para inicializar campo

portais :: [Portal]
portais = []        -- valor para inicializar campo

mapa :: Mapa
mapa = []           -- valor para inicializar campo

loja :: Loja
loja = []           -- valor para inicializar campo

jogoVivo :: Jogo
jogoVivo = Jogo { baseJogo = Base {posicaoBase=(0,0), creditosBase=0, vidaBase = 10 }, inimigosJogo = [], torresJogo=torres, portaisJogo=portais, mapaJogo=mapa, lojaJogo=loja}

jogoPerdido :: Jogo
jogoPerdido = Jogo { baseJogo = Base {posicaoBase=(0,0), creditosBase=0, vidaBase = 0 }, inimigosJogo = [], torresJogo=torres, portaisJogo=portais, mapaJogo=mapa, lojaJogo=loja}

jogoGanho :: Jogo
jogoGanho = Jogo { baseJogo = Base {posicaoBase=(0,0), creditosBase=0, vidaBase = 10} , inimigosJogo = [], portaisJogo = [Portal {posicaoPortal=(0,0), ondasPortal = [Onda {cicloOnda=0, entradaOnda=0, tempoOnda=0, inimigosOnda = []}]}], torresJogo=torres, mapaJogo=mapa, lojaJogo=loja}

jogoNganho :: Jogo
jogoNganho = Jogo { baseJogo = Base {posicaoBase=(0,0), creditosBase=0, vidaBase = 10}, inimigosJogo = [Inimigo {posicaoInimigo=(0,0), direcaoInimigo=Sul, velocidadeInimigo=0, ataqueInimigo=0, butimInimigo=0, projeteisInimigo=[], vidaInimigo = 10, tipoInimigo=Normal, animacao=Walk, frameInimigo=1, tempoAInimigo=0, caminhoInimigo=[], posMedia=Nothing}], portaisJogo = [Portal {posicaoPortal=(0,0), ondasPortal = [Onda {cicloOnda=0, entradaOnda=0, tempoOnda=0, inimigosOnda =[Inimigo {posicaoInimigo=(0,0), direcaoInimigo=Sul, velocidadeInimigo=0, ataqueInimigo=0, butimInimigo=0, projeteisInimigo=[], vidaInimigo = 10, tipoInimigo=Normal, animacao=Walk, frameInimigo=1, tempoAInimigo=0,caminhoInimigo=[], posMedia=Nothing}] }]}], torresJogo=torres, mapaJogo=mapa, lojaJogo=loja}

jogoNganho' :: Jogo
jogoNganho' = Jogo { baseJogo = Base {posicaoBase=(0,0), creditosBase=0, vidaBase = 0}, inimigosJogo = [], portaisJogo = [Portal {posicaoPortal=(0,0), ondasPortal = [Onda {cicloOnda=0, entradaOnda=0, tempoOnda=0, inimigosOnda = []}]}], torresJogo=torres, mapaJogo=mapa, lojaJogo=loja}

jogoTerminado' :: Jogo
jogoTerminado' = Jogo { baseJogo = Base {posicaoBase=(0,0), creditosBase=0, vidaBase = 10} , inimigosJogo = [], portaisJogo = [Portal {posicaoPortal=(0,0), ondasPortal = [Onda {cicloOnda=0, entradaOnda=0, tempoOnda=0, inimigosOnda = []}]}], torresJogo=torres, mapaJogo=mapa, lojaJogo=loja}

jogoTerminado'' :: Jogo
jogoTerminado'' = Jogo { baseJogo = Base {posicaoBase=(0,0), creditosBase=0, vidaBase = 0}, inimigosJogo = [Inimigo {posicaoInimigo=(0,0), direcaoInimigo=Sul, velocidadeInimigo=0, ataqueInimigo=0, butimInimigo=0, projeteisInimigo=[], vidaInimigo = 10, tipoInimigo=Normal, animacao=Walk, frameInimigo=1, tempoAInimigo=0, caminhoInimigo=[], posMedia=Nothing}], portaisJogo = [Portal {posicaoPortal=(0,0), ondasPortal = [Onda {cicloOnda=0, entradaOnda=0, tempoOnda=0, inimigosOnda =[Inimigo {posicaoInimigo=(0,0), direcaoInimigo=Sul, velocidadeInimigo=0, ataqueInimigo=0, butimInimigo=0, projeteisInimigo=[], vidaInimigo = 10, tipoInimigo=Normal, animacao=Walk, frameInimigo=1, tempoAInimigo=0, caminhoInimigo=[], posMedia=Nothing}] }]}], torresJogo=torres, mapaJogo=mapa, lojaJogo=loja}

--jogoTerminado''' :: Jogo
--jogoTerminado''' = Jogo { baseJogo = Base {posicaoBase=(0,0), creditosBase=0, vidaBase = 0}, inimigosJogo = [], portaisJogo = [Portal {posicaoPortal=(0,0), ondasPortal = [Onda {cicloOnda=0, entradaOnda=0, tempoOnda=0, inimigosOnda = []}]}], torresJogo=torres, mapaJogo=mapa, lojaJogo=loja} 

torreTeste :: Torre
torreTeste = Torre {rajadaTorre=0, cicloTorre=0, tempoTorre=0 , danoTorre = 5, projetilTorre = Projetil {duracaoProjetil=Infinita, tipoProjetil = Fogo}, posicaoTorre = (0, 0), alcanceTorre = 10 }
inimigoInicial :: Inimigo
inimigoInicial = Inimigo {direcaoInimigo=Sul, velocidadeInimigo=0, ataqueInimigo=0, butimInimigo=0, vidaInimigo = 10, projeteisInimigo = [], posicaoInimigo = (1, 1), tipoInimigo=Normal, animacao=Walk, frameInimigo=1, tempoAInimigo=0, caminhoInimigo=[], posMedia=Nothing}
inimigoEsperado :: Inimigo
inimigoEsperado = Inimigo {direcaoInimigo=Sul, velocidadeInimigo=0, ataqueInimigo=0, butimInimigo=0, vidaInimigo = 5, projeteisInimigo = [Projetil {duracaoProjetil=Infinita, tipoProjetil = Fogo}], posicaoInimigo = (1, 1), tipoInimigo=Normal, animacao=Walk, frameInimigo=1, tempoAInimigo=0, caminhoInimigo=[], posMedia=Nothing}

portalTeste :: Portal
portalTeste = Portal {posicaoPortal = (10,0), ondasPortal = [Onda {inimigosOnda = [Inimigo {direcaoInimigo=Sul, velocidadeInimigo=0, ataqueInimigo=0, butimInimigo=0, projeteisInimigo=[] , vidaInimigo = 10, posicaoInimigo = (10,0), tipoInimigo=Normal, animacao=Walk, frameInimigo=1, tempoAInimigo=0, caminhoInimigo=[], posMedia=Nothing}], cicloOnda = 10.2, tempoOnda = 2, entradaOnda = 10}] }
inimigosAtivos' :: [Inimigo]
inimigosAtivos' = []
portalSinimigo :: (Portal, [Inimigo])
portalSinimigo = (Portal {posicaoPortal = (10,0), ondasPortal = [Onda {inimigosOnda = [], cicloOnda = 10.2, tempoOnda = 2, entradaOnda = 10}] }, [Inimigo {direcaoInimigo=Sul, velocidadeInimigo=0, ataqueInimigo=0, butimInimigo=0, projeteisInimigo=[], vidaInimigo = 10, posicaoInimigo = (10,0), tipoInimigo=Normal, animacao=Walk, frameInimigo=1, tempoAInimigo=0, caminhoInimigo=[], posMedia=Nothing}] )

-- Testes de HUnit
testesTarefa2 :: Test
testesTarefa2 =
  TestLabel "Testes Tarefa 2" $
    test
      [
        "jogo nao perdeu quando a vida da base é maior que 0(perdeujogo)" ~:
          False ~=? perdeuJogo jogoVivo,

        "jogo perdeu quando a vida da base é 0 ou menor(perdeujogo)" ~:
          True ~=? perdeuJogo jogoPerdido,

        "jogo ganho, vidabase > 0 e sem inimigos ativos(ganhoujogo)" ~:
          True ~=? ganhouJogo jogoGanho, --rever os dados dessa 

        "Jogo nao foi ganho, ainda tem inimigos ativos(ganhoujogo)" ~:
        False ~=? ganhouJogo jogoNganho,

        "Jogo nao foi ganho, a vida da base está menor que zero mesmo sem inimigos ativos(ganhouJogo)" ~:
        False ~=? ganhouJogo jogoNganho',

        "Jogo terminou, logo voce ganhou o jogo(terminouJogo)" ~:
        True ~=? terminouJogo jogoTerminado', --mesmo problema da primeira o problema esta nos dados do HUnit da primeira

        "Jogo terminou, logo voce perdeu o jogo. Ainda existem inimigos(terminouJogo)" ~:
        True ~=? terminouJogo jogoTerminado'',

        "Jogo terminou, logo voce perdeu o jogo. Vida da base < 0, sem inimigos(terminouJogo)" ~:
        True ~=? terminouJogo jogoTerminado'',

        "O inimigo foi ativado(ativaInimigo)" ~:
        portalSinimigo ~=? ativaInimigo portalTeste inimigosAtivos',

        "Inimigo atingido(atingeInimigo)" ~:
         inimigoEsperado ~=? atingeInimigo torreTeste inimigoInicial,

         "O inimigo está no alcance(inimigosNoAlcance)" ~:
        [Inimigo {direcaoInimigo=Sul, velocidadeInimigo=0, ataqueInimigo=0, butimInimigo=0, projeteisInimigo=[], vidaInimigo=10, posicaoInimigo = (3,6), tipoInimigo=Normal, animacao=Walk, frameInimigo=1, tempoAInimigo=0, caminhoInimigo=[], posMedia=Nothing}] ~=?
        inimigosNoAlcance (Torre {danoTorre=0, rajadaTorre=0, projetilTorre=Projetil Fogo Infinita, cicloTorre=0, tempoTorre=0 ,posicaoTorre = (2,4), alcanceTorre = 10.2}) [Inimigo {direcaoInimigo=Sul, velocidadeInimigo=0, ataqueInimigo=0, butimInimigo=0, projeteisInimigo=[], vidaInimigo=10, posicaoInimigo = (3,6), tipoInimigo=Normal, animacao=Walk, frameInimigo=1, tempoAInimigo=0, caminhoInimigo=[], posMedia=Nothing}]
      ]


