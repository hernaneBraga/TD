#Limpando o ambiente
rm(list=ls())

#Importando Bibliotecas e Arquivos necessários
library('matrixcalc')
#source('solucao_inicial.R')
source('solucao_inicial_2.R')
source('sol_inicial_gulosa.R')
source('vizinhanca.R')

#Lendo os dados
distancias <- as.matrix(read.csv(file="distancia.csv", header=FALSE, sep=","))

#Solução inicial pelo algoritmo guloso
X <- sol_inicial("distancia.csv")

######################
#Escolhe 100 vizinhos próximos e calcula a temperatura inicial
tau <- 0.5
x1 <- X
deltaE <- NULL
for (i in 1:100){
  deltaE <- c(deltaE,((sum(Vizinhanca(x1, distancias, j)$custo)) - (sum(X$custo))))
}
T0 <- -mean(deltaE)/(log(tau))
Tk <- T0
nivel <- 1
deltaE <- 0
m <- 1

#Loop do SA
seqi <- seq(0.001,1,0.001)
costt <- sum(X$custo)
iteracao <- 0
nivel <- 1

while (iteracao < 10000 && Tk > 0.001*T0 && nivel<=7){
    aceitacao <- 0
    m <- 0
    while (m <= 200 && aceitacao<=12){
      cost1 <- sum(X$custo)
      x1 <- Vizinhanca(X, distancias, nivel)
      cost2 <- sum(x1$custo)
      deltaE <- cost2 - cost1
        while (deltaE == 0){
          cost1 <- sum(X$custo)
          x1 <- Vizinhanca(X, distancias, nivel)
          cost2 <- sum(x1$custo)
          deltaE <- cost2 - cost1
       }
      if (deltaE <= 0){
        aceitacao <- aceitacao +1
        X <- x1
        costt <- c(costt,cost2)
      } else {
        prob <- exp(-deltaE/Tk)
        if (sample(seqi,1)<prob) {
          X <- x1
          costt <- c(costt,cost2)
          aceitacao <- aceitacao +1
        }
      }
      m <- m+1
    }
    if (m == 100) {
      Tk <- Tk*0.90
      nivel <- nivel+1
    }
    if (aceitacao > 1){
      Tk <- 0.999*Tk
      nivel <- 1
    }
    iteracao <- iteracao+1
    #cat("iteracao:",iteracao,"\n")
}
custom <- sum(X$custo)
plot(costt, type="line")