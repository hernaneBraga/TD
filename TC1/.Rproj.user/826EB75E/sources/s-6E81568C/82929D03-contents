#Universidade Federal de Minas Gerais
#Introducao ao Reconhecimento de Padroes
#Nikolas Dias Magalhaes Fantoni
#AULA 11 e 12 - Espaço de Verossimilhanças (parte 1)
#2019/2

#Limpando o ambiente
rm(list=ls())

#Adicionando biblioteca
library('caret')
library('mlbench')
library('plot3D')
source('escolhefold.R')

#Criando dados
nc <- 1000
Y<-mlbench.spirals(nc,cycles=1, sd=0.05)
classes <- 2
X <- cbind(Y$x, Y$classes)
fl <- createFolds(X[,1], k = 10, list = TRUE, returnTrain = FALSE)

#loop para definir o melhor fold
acuracia <- NULL
for (i in 1:10){
  test <- X[fl[[i]],]
  train <- X[-fl[[i]],]
#Definindo constantes do teste
  errado <- 1
  k<-4
  
#k-means
  while (errado != 0 && k < 40){
    errado <- 0
    k <- k+1
    
    w <- kmeans(train[,1:2],k,iter.max = 20)
    media <- NULL
  
    for (i in 1:k){
      newseq <- which(w$cluster==i)
      media <- mean(as.numeric(train[newseq,3]))
      if (media > as.numeric(train[newseq[1],3])*1.02 || media < as.numeric(train[newseq[1],3])*0.98) errado <- errado + 1
    }
  }
  


##############################################

#funcao para estimativa da densidade de 2 variáveis
pdf2var<-function(x,y,u1,u2,s1,s2,p) {(1/(2*pi*s1*s2*sqrt(1-(p^2))))*
    exp((-(1)/(2*(1-(p^2))))*((((x-u1)^2)/((s1)^2))+(((y-u2)^2)/((s2)^2))-
                                ((2*p*(x-u1)*(y-u2))/(s1*s2))))
}

#cáculo da média e desvio padrão das classes
clusters <- list()
c1 <- list()
c2 <- list()
count1 <- 1
count2 <- 1
for (i in 1:k){
  seqc <- which(w$cluster==i)
  clusters[[i]] <- train[seqc,]
  if (train[seqc[1],3]==1){
    c1[[count1]] <- train[seqc,]
    count1 <- count1+1
  } else {
    c2[[count2]] <- train[seqc,]
    count2 <- count2+1
  }
}

s1 <- list()
s2 <- list()
pc1 <- list()
pc2 <- list()
for (i in 1:length(c1)){
  s1[[i]] <- cbind(sd(c1[[i]][,1]),sd(c1[[i]][,2]))
  pc1[[i]] <- cov(c1[[i]][,1],c1[[i]][,2])/sqrt(prod(s1[[i]]^2))
}
for (i in 1:length(c2)){
  s2[[i]] <- cbind(sd(c2[[i]][,1]),sd(c2[[i]][,2]))
  pc2[[i]] <- cov(c2[[i]][,1],c2[[i]][,2])/sqrt(prod(s2[[i]]^2))
}


#Classificando as amostras de teste
totalc1 <- NULL
totalc2 <- NULL
for (m in 1:length(c1)){
  totalc1 <- c(totalc1,length(c1[[m]][,1]))
}
for (m in 1:length(c2)){
  totalc2 <- c(totalc2,length(c2[[m]][,1]))
}
totalc1 <- sum(totalc1)
totalc2 <- sum(totalc2)

erro <- 0
f1t <- NULL
f2t <- NULL
for (i in 1:length(test[,1])){
  c <- 0
  l <- t(test[i,1:2])
  f1 <- NULL
  f2 <- NULL
  for (m in 1:length(c1)){
    pir <- length(c1[[m]][,1])/totalc1
    f1<- rbind(f1,pir*pdf2var(l[,1],l[,2],mean(c1[[m]][,1]),mean(c1[[m]][,2]),s1[[m]][1,1],s1[[m]][1,2],pc1[[m]]))
  }
  for (m in 1:length(c2)){
    pir <- length(c2[[m]][,1])/totalc2
    f2<- rbind(f2,pir*pdf2var(l[,1],l[,2],mean(c2[[m]][,1]),mean(c2[[m]][,2]),s2[[m]][1,1],s2[[m]][1,2],pc2[[m]]))
  }
  f1 <- sum(f1)
  f2 <- sum(f2)
  f1t <- c(f1t,f1)
  f2t <- c(f2t,f2)
  if (f2 == 0){
    c <- 1
  } else {
    c <- if ((f1/f2 > 1) == TRUE) 1 else 2
    if ((c-test[i,3]) != 0) erro <- erro +1
  }
}
acuracia <- c(acuracia, erro/length(test[,1]))
}

#plotando
cores <- rainbow(k)
#Dados de Entrada
plot(Y, xlim = c(-1,1),ylim = c(-1,1), xlab="x1", ylab="x2")

#Clusters
for (i in 1:k){
  plot(clusters[[i]][,1],clusters[[i]][,2], col=cores[i],  xlim = c(-1.2,1.2),ylim = c(-1.2,1.2),
       xlab = 'x1' ,ylab= 'x2' )
  par(new="T")
}
for (i in 1:k){
  plot(w$centers[,1],w$centers[,2], col='black',  xlim = c(-1.2,1.2),ylim = c(-1.2,1.2), pch=3,
       xlab = '' ,ylab= '' )
  par(new="T")
}
par(new="F")

#Espaço Verossemelhança
for (i in 1:length(f1t)){
if (f1t[i]<=f2t[i]) plot(f1t[i], f2t[i], col="red", xlim=c(0,3), ylim=c(0,3.2), xlab="p(x|C1)", ylab="p(x|C2)")
if (f2t[i]<f1t[i]) plot(f1t[i], f2t[i], col="blue", xlim=c(0,3), ylim=c(0,3.2), xlab="p(x|C1)", ylab="p(x|C2)")
par(new="T")
}
par(new="F")

#Curva de Separacao
iseq <- seq(0,3,0.01)
jseq <- (totalc1/totalc2)*iseq
for (i in 1:length(f1t)){
  if (f1t[i]<=f2t[i]) plot(f1t[i], f2t[i], col="red", xlim=c(0,3), ylim=c(0,3.2), xlab="p(x|C1)", ylab="p(x|C2)")
  if (f2t[i]<f1t[i]) plot(f1t[i], f2t[i], col="blue", xlim=c(0,3), ylim=c(0,3.2), xlab="p(x|C1)", ylab="p(x|C2)")
  par(new="T")
}
plot(iseq,iseq,type="line", xlim=c(0,3), ylim=c(0,3.2), xlab="p(x|C1)", ylab="p(x|C2)")
par(new="F")

