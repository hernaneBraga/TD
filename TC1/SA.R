#Limpando o ambiente
rm(list=ls())

library('matrixcalc')
source('newsolution.R')

#Lendo os dados
distancias <- as.matrix(read.csv(file="distancia.csv", header=FALSE, sep=","))
is.symmetric.matrix(distancias)

#Gerando solução inicial
x <- matrix(0,nrow=250,ncol=250)
iseq <- sample(250)
x[1,iseq[1]]<-1
for (i in 1:249){
    x[iseq[i],iseq[i+1]] <- 1
}
x[iseq[250],1] <- 1
custo <- sum(x * distancias)
iteracao <- 0
possiveis <- 1:250
possiveis <- possiveis[-iseq[250]]
possiveis <- possiveis[-1]
possiveis <- possiveis[-iseq[249]]

#Gerando nova solução
while (iteracao < 1000){
  iteracao <- iteracao + 1
  n <- sample(possiveis,1)
  y <- newsolution(x,n)
  custonovo <- sum(y * distancias)
  if (custonovo < custo) {
    custo <- custonovo
    x <- y
  }
}