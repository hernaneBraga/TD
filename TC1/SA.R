#Limpando o ambiente
rm(list=ls())

#Importando Bibliotecas e Arquivos necessários
library('matrixcalc')
source('newsolution.R') #Coloca como Source o arquivo que contem a solucao newsolution (vizinhança)
source('solucao_inicial.R')
source('vizinhanca.R')

#Lendo os dados
distancias <- as.matrix(read.csv(file="distancia.csv", header=FALSE, sep=","))

#Estabelece solução inicial
X <- solucao_incial("distancia.csv")
destino <- X[1,]
custo <- X[2,]
X <- data.frame(cbind(destino, custo))

#Escolhe 100 vizinhos próximos e calcula a temperatura inicial
tau <- 0.5
x1 <- X
deltaE <- NULL
for (i in 1:100){
  deltaE <- c(deltaE,(sum(X$custo) - sum(Vizinhanca(x1, distancias, 1)$custo)))
}
T0 <- -mean(deltaE)/tau

#Loop do SA
custoso <- sum(X$custo)
x1 <- Vizinhanca(X, distancias, 1)
custoso2 <- sum(x1$custo)



##########################################################

#Recebe a solução e gera novas para o calculo do deltaE

#Gerando nova solução
iteracao <- 1000
while (iteracao < 1000){
  iteracao <- iteracao + 1
  y <- newsolution(x,n)
  custonovo <- sum(y * distancias)
  if (custonovo < custo) {
    custo <- custonovo
    x <- y
  }
}