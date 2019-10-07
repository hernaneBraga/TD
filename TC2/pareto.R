source('SA.R')
source('SAmulti.R')
source('ConfereDominancia.R')

SomaPonderada <- function(X, dados_custo_tempo, dados_custo_distancia, minimos, maximos){
  minimos <- c(16.5,1225)
  maximos <- c(267,13043)
  #Pega os menores custos conhecidos
  mint <- minimos[1]
  mind <- minimos[2]
  maxt <- maximos[1]
  maxd <- maximos[2]
  a <- list()
  a$destino <- X$destino
  a$custos <- X$custodistancia
  a<- data.frame(a)
  Zd <- SA(a,dados_custo_distancia)
  temp <- NULL
  for (i in 1:length(Zd$destino)){
    temp <- c(temp,as.numeric(dados_custo_tempo[i,Zd$destino[i]]))
  }
  Zd$custotempo <- temp
  temp <- Zd$custos
  Zd$custos <- NULL
  Zd$custodistancia <- temp
  a <- list()
  a$destino <- X$destino
  a$custos <- X$custotempo
  a<- data.frame(a)
  Zt <- SA(a,dados_custo_tempo)
  temp <- NULL
  for (i in 1:length(Zt$destino)){
    temp <- c(temp,as.numeric(dados_custo_distancia[i,Zt$destino[i]]))
  }
  Zt$custodistancia <- temp
  temp <- Zt$custos
  Zt$custos <- NULL
  Zt$custotempo <- temp

  solucoes <- NULL
  Z2 <- Zd
  Z1 <- Zt
  for (ii in 1:10){
  lambdad <- normalizando(sum(Z1$custodistancia),mind,maxd) - normalizando(sum(Z2$custodistancia),mind,maxd)
  lambdat <- normalizando(sum(Z2$custotempo),mint,maxt) - normalizando(sum(Z1$custotempo),mint,maxt)
  z <- SAmulti(X, dados_custo_tempo, dados_custo_distancia, wd=lambdad, wt=lambdat,maxit=1000)[[1]]
  solucoes <- cbind(solucoes,c(sum(z$custotempo),sum(z$custodistancia)))
  Z2 <- Z1
  Z1 <- z
  }
  Z2 <- Zd
  Z1 <- Zt
  for (ii in 1:10){
  lambdad <- normalizando(sum(Z1$custodistancia),mind,maxd) - normalizando(sum(Z2$custodistancia),mind,maxd)
  lambdat <- normalizando(sum(Z2$custotempo),mint,maxt) - normalizando(sum(Z1$custotempo),mint,maxt)
  z <- SAmulti(X, dados_custo_tempo, dados_custo_distancia, wd=lambdad, wt=lambdat,maxit=1000)[[1]]
  solucoes <- cbind(solucoes,c(sum(z$custotempo),sum(z$custodistancia)))
  Z1 <- Z2
  Z2 <- z
  }
  
  final <- ConfereDominancia(solucoes)
  plot(solucoes[1,],solucoes[2,])
  
}