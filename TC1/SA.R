#Limpando o ambiente
rm(list=ls())

#Importando Bibliotecas e Arquivos necessários
library('matrixcalc')
source('solucao_inicial.R')
source('vizinhanca.R')

#Lendo os dados
distancias <- as.matrix(read.csv(file="distancia.csv", header=FALSE, sep=","))

#Estabelece solução inicial
#X <- solucao_incial("distancia.csv")
#destino <- X[1,]
#custo <- X[2,]
#Y <- data.frame(cbind(destino, custo))
#cat(sum(X$custo))


#Segunda solucao inicial
X<-list()
X[[1]] <- sample(2:250,1)
X[[2]] <- as.numeric(distancias[1,X[[1]]])
listapossiveis <- 2:250
listapossiveis <- listapossiveis[-(X[[1]][1]-1)]
names(X) <- c("destino", "custo")
custosi <- list()

for (i in 1:248){
custosi[[1]] <- as.numeric(distancias[X[[1]][i],listapossiveis])
ordem <- order(custosi[[1]])
custosi[[1]] <- custosi[[1]][ordem]
custosi[[2]] <- listapossiveis[ordem]
indice <- custosi[[2]][1]
X[[1]] <- c(X[[1]],indice)
X[[2]] <- c(X[[2]],custosi[[1]][1])
listapossiveis <- listapossiveis[-which(listapossiveis==indice)]
  }
X[[1]] <- c(X[[1]],1)
X[[2]] <- c(X[[2]],as.numeric(distancias[X[[1]][249],1]))
rm(custosi,indice, listapossiveis, ordem)

custo <- sum(X[[2]])
X <- data.frame(X)

#Escolhe 100 vizinhos próximos e calcula a temperatura inicial
tau <- 0.5
x1 <- X
deltaE <- NULL
for (j in 1:6){
for (i in 1:100){
  deltaE <- c(deltaE,((sum(Vizinhanca(x1, distancias, j)$custo)) - (sum(X$custo))))
}
cat(mean(deltaE), "\n")
}
T0 <- -mean(deltaE)/(log(tau))
Tk <- T0
nivel <- 1
deltaE <- 0
m <- 1

#Loop do SA
seqi <- seq(0.01,1,0.001)
melhor <- 0
costt <- sum(X$custo)

while (nivel < 7){
    aceitacao <- 0
    while (m <= 100 && aceitacao <=12){
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
        nivel <- 1
        aceitacao <- aceitacao +1
        X <- x1
        costt <- c(costt,cost2)
      } else {
        melhor <- 0
        prob <- exp(-deltaE/Tk)
        if (sample(seqi,1)<prob) {
          X <- x1
          costt <- c(costt,cost2)
          aceitacao <- aceitacao +1
        }
      }
      m <- m+1
    }
    Tk <- Tk*0.99 
    if (Tk < 0.01){
      nivel <- nivel+1
    }
    if (aceitacao>12 && nivel>1){
      Tk <- T0
      nivel <- 1
    }
}

custom <- sum(X$custo)
plot(costt, type="line")


nivel <- 1
for (i in 1:10000){
cost1 <- sum(X$custo)
x1 <- Vizinhanca(X, distancias, nivel)
cost2 <- sum(x1$custo)
deltaE <- cost1 - cost2
if (deltaE == 0) break else X <- x1
}