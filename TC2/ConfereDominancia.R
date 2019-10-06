library(dplyr)

ConfereDominancia <- function(solucoes){
  rownames(solucoes) <- c("tempo","distancia")
  solucoes <- data.frame(t(solucoes))
  solucoes <- distinct(solucoes, tempo, distancia, .keep_all = TRUE)
  solucoes <- t(as.matrix(solucoes))
  
  final <- matrix(solucoes[,1],nrow=2,ncol=1)
  for (ii in 2:length(solucoes[1,])){
    breakpoint <- length(final[1,])
    jj <- 1
    candidata <- 0
    while (jj <= breakpoint){
      if((solucoes[1,ii] > final[1,jj] && solucoes[2,ii] < final[2,jj])||
         (solucoes[1,ii] < final[1,jj] && solucoes[2,ii] > final[2,jj])){
        candidata <- candidata+1
      } 
      if((solucoes[1,ii] <= final[1,jj] && solucoes[2,ii] < final[2,jj])||
        (solucoes[1,ii] < final[1,jj] && solucoes[2,ii] <= final[2,jj])){
        final <- matrix(solucoes[,ii],nrow=2,ncol=1)
        jj <- breakpoint
      }
      jj <- jj+1
    }
    if (candidata == length(final[1,])){
      final <- cbind(final,solucoes[,ii])
    }
  }
  return(final)
}