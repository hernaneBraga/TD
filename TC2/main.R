###################################################################
# UNIVERSIDADE FEDERAL DE MINAS GERAIS                            
# BACHARELADO EM ENGENHARIA DE SISTEMAS                           
# DISCIPLINA: ELE088 Teoria da Decisao                            
# PROFESSOR: Lucas de Souza Batista                               
# ALUNOs: Ariel Domingues, Hernane Braga e Nikolas Fantoni        
# DATA: Outubro/2019                               
# TC1 - Otimizacao multi-objetivo do PCV

# O algoritmo abaixo pode ser rodado como um todo ou em blocos, conforme dividido a seguir.
# É aconselhável a limpeza do ambiente antes da execução do algoritmo, usando a função abaixo.
rm(list=ls())

# Importando Bibliotecas, arquivos necessários e os dados
# Aqui deve ser definido se o arquivo distancia.csv ou o arquivo tempo.csv
# serão utilizados na otimizacao.
source('solucao_inicial.R')
source('vizinhanca.R')
source('SA.R')
dados_custo_distancia <- as.matrix(read.csv(file="distancia.csv", header=FALSE, sep=","))
dados_custo_tempo <- as.matrix(read.csv(file="tempo.csv", header=FALSE, sep=","))


###################################################
###          BLOCO DAS SOLUÇÕES INICIAIS        ###
###################################################

# Define o grau de variabilidade das soluções iniciais (número inteiro).
# Valores menores significam uma solução mais próxima da solução obtida
# a partir de um algoritmo guloso.

grau <- 1
Xd <- t(solucao_inicial("distancia.csv",grau))
colnames(Xd) <- c("destino", "custo")
Xt <- t(solucao_inicial("tempo.csv",grau))
colnames(Xt) <- c("destino", "custo")
Xd <- data.frame(Xd)
Xt <- data.frame(Xt)
initcost_t <- sum(Xt$custo) #define custo inicial para o tempo
initcost_d <- sum(Xd$custo) #define custo inicial para a distância
# Para imprimir o custo inicial
# cat("Custo inicial para o tempo: ", initcost_t, "h.\nCusto inicial para a distância: ", initcost_d,"km. \n")

###################################################
###     BLOCO DO SIMULATED ANNEALING INICIAL    ###
###################################################

# Realiza o SA
xbest_t <- SA(X=Xt,dados_custo_tempo)
xbest_d <- SA(X=Xd,dados_custo_distancia)

# Salva o custo final da melhor solução encontrada
custod <- sum(xbest_d$custo)
custot <- sum(xbest_t$custo)

# Limpa variáveis não mais úteis
rm(grau,Xd, Xt)

###################################################
###             BLOCO DAS SOLUÇÕES              ###
###################################################

# Plota custos e imprime o custo final e o custo inicial
plot(1:length(costt), costt,type="l", xlab="",ylab="Custo", main="Evolução do custo ao longo do algoritmo SA")
cat("Custo inicial: ", custoinicial, ".\n")
cat("Custo final: ", custofinal,".\n")