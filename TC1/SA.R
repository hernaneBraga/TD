#Limpando o ambiente
rm(list=ls())

#Importando Bibliotecas e Arquivos necessários
library('matrixcalc')
source('newsolution.R') #Coloca como Source o arquivo que contem a solucao newsolution (vizinhança)

#Lendo os dados
distancias <- as.matrix(read.csv(file="distancia.csv", header=FALSE, sep=","))

#Gerando solução inicial
#x <- solucaoinicial(distancias) #Coloca para receber uma solução inicial

x <- matrix(0,nrow=2,ncol=250)
iseq <- sample(250)
x[1,1] <- 1
x[1,2:250] <- iseq[-which(iseq == 1)]
for (i in 1:249){
  x[2,i] <- distancias[x[1,i],x[1,i+1]] 
}
x[2,250] <- distancias[x[1,250],1]

custo <- sum(x[2,])

########################################################
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