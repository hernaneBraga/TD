
Xd <- xbest_d
Xt <- xbest_t
minimos <- c(16.5,1225)
maximos <- c(267,13043)


SomaPonderada <- function(Xinter, Xt, Xd, dados_custo_tempo, dados_custo_distancia, minimos, maximos){
  
  #Pega os menores custos conhecidos
  mint <- minimos[1]
  mind <- minimos[2]
  maxt <- maximos[1]
  maxd <- maximos[2]
  
  #calcula os maiores custos
  invt <- 0
  invd <- 0
  for (i in 1:length(Xd$destino)){
    invt <- invt + dados_custo_tempo[i,Xd$destino[i]]
    invd <- invd + dados_custo_distancia[i,Xt$destino[i]]
  }
  invt <- as.numeric(invt)
  invd <- as.numeric(invd)
  
  custonormt <- normalizando(sum(Xt$custo),mint, maxt)
  custonormd <- normalizando(sum(Xd$custo),mind, maxd)
  kd <- normalizando(invd,mind, maxd)
  kt <- normalizando(invt,mint, maxt)
  lambdad <- kt - custonormt
  lambdat <- kd - custonormd
  z <- SAmulti(Xinter, lamndat, lambdad, dados_custo_tempo, dados_custo_distancia )
  
  
  
}