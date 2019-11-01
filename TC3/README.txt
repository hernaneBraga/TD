#############################################################################
UNIVERSIDADE FEDERAL DE MINAS GERAIS
BACHARELADO EM ENGENHARIA DE SISTEMAS
DISCIPLINA: ELE088 Teoria da Decisao
PROFESSOR: Lucas de Souza Batista
ALUNOs: Ariel Domingues, Hernane Braga e Nikolas Fantoni
DATA: Novembro/2019
TC3 - Tomada de Decisão Multicritério no PCV

##############################################################################
INTRODUCAO

O TC3 consiste na tomada de decisao dado que as alternativas ja foram capturadas anteriormente.

As alternativas utilizadas e analisadas neste trabalho estao contidas no arquivo "alternativas.txt".
Tal arquivo contem as 11 alternativas, e os metodos de decisao foram desenvolvidos para ler arquivos com esse nome e neste estilo.

##############################################################################
OPCIONAL

Caso queira gerar novas alternativas eh preciso rodar o arquivo "main.R" com todos os arquivos que ele necessita na mesma pasta.
Eles sao: "solucao_inicial.R", 
          "vizinhanca2.R", 
          "SAmultiSP.R", 
          "ConfereDominancia.R", 
          "EncontraSolucoes.R",
e os arquivos de dados: "distancia.csv" e "tempo.csv".

Esses arquivos representam o que foi desenvolvido no TC1 e TC2.

OBS: A geracao dessas novas solucoes Pareto-otimas pode demorar e gerar poucas solucoes.

ATENCAO: caso isso seja feito, o arquivo "alternativas.txt" sera sobrescrito com as novas alternativas.

################################################################################
AHP

Para aplicar o metodo AHP basta rodar o arquivo "ahp.R" na mesma pasta que o arquivo "alternativas.txt".

A saida mostrara no terminal as seguintes mensagens, com a ordem das alternativas (no caso, um exemplo):

"The best alternative in this configuration is:  1"
"The order of preference:  1 4 8 9 7 10 2 11 6 3 5"

Caso queira ver as matrizes definidas no metodo basta digitar no terminal "criteria_matriz" e "alternatives_matrix" apos ter rodado o codigo. 
As prioridades ja estao inseridas na ultima coluna de cada matriz (as linhas e colunas estao nomeadas).

As inconsistencias podem ser vistas digitando "criteria_inconsistency" e "alternatives_inconsistency".

Os valores das alternativas em cada criterio podem ser vistos digitando "criteria" no terminal (o que eh valido nos outros metodos tambem, dado a padronizacao).

#################################################################################
ELECTRE I

Para aplicar o metodo ELECTRE I basta rodar o arquivo "electreI.R" na mesma pasta que o arquivo "alternativas.txt".

A saida sera a matriz de sobreclassificacao mostrada no terminal (exemplo):

      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11]
 [1,]    0    1    1    1    1    0    0    0    0     0     0
 [2,]    0    0    0    0    0    0    0    0    0     0     0
 [3,]    0    0    0    0    1    0    0    0    0     0     0
 [4,]    0    1    0    0    0    0    0    0    0     0     0
 [5,]    0    0    0    0    0    0    0    0    0     0     0
 [6,]    0    0    1    0    1    0    0    0    0     0     1
 [7,]    0    0    0    0    0    0    0    0    0     0     0
 [8,]    0    0    0    0    0    0    0    0    0     0     0
 [9,]    0    0    0    0    0    0    0    0    0     0     0
[10,]    0    0    0    0    0    0    0    1    1     0     0
[11,]    0    0    0    0    0    0    0    0    0     0     0

Para ver a matriz de concordancia e de discordancia basta digitar no terminal "Concordancia" e "Discordancia", respectivamente, apos ter rodado o codigo.

##################################################################################
PROMETHEE II

Para aplicar o metodo PROMETHEE II basta rodar o arquivo "prometheeII.R" na mesma pasta que o arquivo "alternativas.txt".

A saida sera a matriz de sobreclassificacao mostrada no terminal assim como as mensagens e a ordem das alternativas (igual ao AHP):

      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11]
 [1,]    0    1    1    1    1    1    1    1    1     1     1
 [2,]    0    0    1    0    1    0    1    1    1     0     1
 [3,]    0    0    0    0    1    0    1    0    0     0     1
 [4,]    0    1    1    0    1    1    1    1    1     1     1
 [5,]    0    0    0    0    0    0    0    0    0     0     1
 [6,]    0    1    1    0    1    0    1    1    1     0     1
 [7,]    0    0    0    0    1    0    0    0    0     0     1
 [8,]    0    0    1    0    1    0    1    0    0     0     1
 [9,]    0    0    1    0    1    0    1    0    0     0     1
[10,]    0    1    1    0    1    1    1    1    1     0     1
[11,]    0    0    0    0    0    0    0    0    0     0     0

"The best alternative in this configuration is:  1"
"The order of preference:  1 4 10 6 2 8 9 3 7 5 11"

As relacoes de preferencia por criterio e globais podem ser vistas digitando no terminal "Pj" e "P", respectivamente, apos ter rodado o codigo.
O fluxo total pode ser visto digitando "fluxo".
