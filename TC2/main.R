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
source('solucao_inicial_dst_tempo.R')
source("solucao_inicial_gulosa.R")
source('vizinhanca2.R')
source('SAmulti.R')
dados_custo_distancia <- as.matrix(read.csv(file="distancia.csv", header=FALSE, sep=","))
dados_custo_tempo <- as.matrix(read.csv(file="tempo.csv", header=FALSE, sep=","))


###################################################
###          BLOCO DAS SOLUÇÕES INICIAIS        ###
###################################################

# Define o grau de variabilidade das soluções iniciais (número inteiro).
# Valores menores significam uma solução mais próxima da solução obtida
# a partir de um algoritmo guloso.

grau <- 5
X <- t(solucao_inicial("distancia.csv","tempo.csv",grau))
dtemp <- X[,2]
X[,2] <- X[,3]
X[,3] <- dtemp
colnames(X) <- c("destino", "custotempo", "custodistancia")
X <- data.frame(X)
X$custotempo[1] <- dados_custo_tempo[1,X$destino[1]] #corrige função do hernane
# Para imprimir o custo inicial
# cat("Custo inicial para o tempo: ", initcost_t, "h.\nCusto inicial para a distância: ", initcost_d,"km. \n")

# Limpa variáveis não mais úteis
rm(grau,dtemp)

###################################################
###     BLOCO DO SIMULATED ANNEALING INICIAL    ###
###################################################
# Realiza o SA
solution <- SAmulti(X,dados_custo_tempo, dados_custo_distancia,0.5,0.5)
xbest <- solution[[1]] # Melhor solução encontrada
custos <- solution[[2]] # Variação do custo ao longo das iterações

#Plota os custos
plot(custos,type='l',ylab="Custos", main="Evolução dos custos ao longo do algoritmo")
