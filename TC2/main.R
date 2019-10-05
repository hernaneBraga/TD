###################################################################
# UNIVERSIDADE FEDERAL DE MINAS GERAIS                            
# BACHARELADO EM ENGENHARIA DE SISTEMAS                           
# DISCIPLINA: ELE088 Teoria da Decisao                            
# PROFESSOR: Lucas de Souza Batista                               
# ALUNOs: Ariel Domingues, Hernane Braga e Nikolas Fantoni        
# DATA: Setembro/2019                               
# TC1 - Otimizacao mono-objetivo do PCV

# O algoritmo abaixo pode ser rodado como um todo ou em blocos, conforme dividido a seguir.
# É aconselhável a limpeza do ambiente antes da execução do algoritmo, usando a função abaixo.
rm(list=ls())

# Importando Bibliotecas, arquivos necessários e os dados
# Aqui deve ser definido se o arquivo distancia.csv ou o arquivo tempo.csv
# serão utilizados na otimizacao.
source('solucao_inicial.R')
source('vizinhanca.R')
source('SA.R')
dados_custo <- as.matrix(read.csv(file="distancia.csv", header=FALSE, sep=","))


###################################################
###           BLOCO DA SOLUÇÃO INICIAL          ###
###################################################

# Define o grau de variabilidade da solução inicial (número inteiro).
# Valores menores significam uma solução mais próxima da solução obtida
# a partir de um algoritmo guloso.

grau <- 1
X <- t(solucao_inicial("distancia.csv",grau))
colnames(X) <- c("destino", "custo")
X <- data.frame(X)
custoinicial <- sum(X$custo) #define custo inicial
# Para imprimir o custo inicial
# cat("Custo inicial: ", custoinicial, ".\n")

###################################################
###       BLOCO DO SIMULATED ANNEALING          ###
###################################################

# Realiza o SA
xbest <- SA(X=X,tau=0.5)

# Salva o custo final da melhor solução encontrada
custofinal <- sum(xbest$custo)

# Limpa variáveis não mais úteis
rm(cost1, cost2, prob, seqi)

###################################################
###             BLOCO DAS SOLUÇÕES              ###
###################################################

# Plota custos e imprime o custo final e o custo inicial
plot(1:length(costt), costt,type="l", xlab="",ylab="Custo", main="Evolução do custo ao longo do algoritmo SA")
cat("Custo inicial: ", custoinicial, ".\n")
cat("Custo final: ", custofinal,".\n")