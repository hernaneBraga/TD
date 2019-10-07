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
source('vizinhanca2.R')
source('SAmultiER.R')
source('SAmultiSP.R')
source('ConfereDominancia.R')
dados_custo_distancia <- as.matrix(read.csv(file="distancia.csv", header=FALSE, sep=","))
dados_custo_tempo <- as.matrix(read.csv(file="tempo.csv", header=FALSE, sep=","))


###################################################
###          BLOCO DAS SOLUÇÕES INICIAIS        ###
###################################################

# Define o grau de variabilidade das soluções iniciais (número inteiro).
# Valores menores significam uma solução mais próxima da solução obtida
# a partir de um algoritmo guloso.

grau <- 1
X <- t(solucao_inicial("distancia.csv","tempo.csv",grau))
dtemp <- X[,2]
X[,2] <- X[,3]
X[,3] <- dtemp
colnames(X) <- c("destino", "custotempo", "custodistancia")
X <- data.frame(X)
X$custotempo[1] <- dados_custo_tempo[1,X$destino[1]] # Corrige função de solução inicial
# Para imprimir o custo inicial
# cat("Custo inicial para o tempo: ", initcost_t, "h.\nCusto inicial para a distância: ", initcost_d,"km. \n")

# Limpa variáveis não mais úteis
rm(grau,dtemp)


###################################################
###      BLOCO DO SIMULATED ANNEALING PW        ###
###################################################

# Variáveis auxiliares
todassaidas <- list()
cores <- rainbow(5)
start_time <- Sys.time()

# Roda o algoritmo cinco vezes, como pedido na descrição do trabalho
for (k in 1:5){
  
# Realiza o SA no método Pw para vários pesos
seqj <- seq(0.02,0.98,0.02)
candidatas <- array(0,c(2,length(seqj)))
contador <- 1
for (j in seqj){
  solution <- SAmultiSP(X,dados_custo_tempo, dados_custo_distancia,wd=j,wt = (1-j), maxit = 1000)
  candidatas[,contador] <- c(sum(solution[[1]]$custotempo),sum(solution[[1]]$custodistancia))
  contador <- contador+1
  }
saida <- ConfereDominancia(candidatas)
todassaidas[[k]] <- saida # Salva todas as saídas Pareto Ótimas de cada iteração

# Plota as superfícies Pareto Ótimas de cada iteração
plot (saida[1,],saida[2,], col=cores[k],xlab="Custo do Tempo", ylab="Custo da Distância", xlim=c(25.5,32), ylim=c(1520,1750))
par(new=T)

}
par(new=F)

end_time <- Sys.time()

###################################################
###      BLOCO DO SIMULATED ANNEALING PE        ###
###################################################

# Variáveis auxiliares
todassaidas <- list()
cores <- rainbow(5)
start_time <- Sys.time()

# Roda o algoritmo cinco vezes, como pedido na descrição do trabalho
for (k in 1:5){

# Realiza o SA no método Pe para vários epsilons
epsilonrange <- seq(1580,1640,2)
candidatas <- array(0,c(2,(length(epsilonrange)*2)))
contador <- 1 
for (epsilon in epsilonrange){
    solution <- SAmultiER(X,dados_custo_tempo, dados_custo_distancia,epsilon = epsilon, maxit = 1000)
    candidatas[,contador] <- c(sum(solution[[1]]$custotempo),sum(solution[[1]]$custodistancia))
    contador <- contador+1
}
epsilonrange <- seq(28,34,0.2)
for (epsilon in epsilonrange){
  solution <- SAmultiER(X,dados_custo_tempo, dados_custo_distancia,epsilon = epsilon, maxit = 1000)
  candidatas[,contador] <- c(sum(solution[[1]]$custotempo),sum(solution[[1]]$custodistancia))
  contador <- contador+1
}
saida <- ConfereDominancia(candidatas)
todassaidas[[k]] <- saida # Salva todas as saídas Pareto Ótimas de cada iteração

plot (saida[1,],saida[2,], col=cores[k],xlab="Custo do Tempo", ylab="Custo da Distância", xlim=c(28,34), ylim=c(1450,1650))
par(new=T)

}
par(new=F)

end_time <- Sys.time()