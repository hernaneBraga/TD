###################################################################
# UNIVERSIDADE FEDERAL DE MINAS GERAIS                            
# BACHARELADO EM ENGENHARIA DE SISTEMAS                           
# DISCIPLINA: ELE088 Teoria da Decisao                            
# PROFESSOR: Lucas de Souza Batista                               
# ALUNOs: Ariel Domingues, Hernane Braga e Nikolas Fantoni        
# DATA: Outubro/2019                               
# TC1 - Otimizacao multi-objetivo do PCV

# O algoritmo abaixo Confere se as soluções encontradas são Pareto-Ótimas
# excluindo as soluções que possuem relação de dominância

# Biblioteca necessária para eliminar duplicatas
library(dplyr)

# Compara todas as soluções, uma a uma, para ver se há dominância
ConfereDominancia <- function(solucoes){
  # Formata a solução inicial, eliminando duplicatas
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
        final[,jj] <- solucoes[,ii] 
      }
      jj <- jj+1
    }
    if (candidata == length(final[1,])){
      final <- cbind(final,solucoes[,ii])
    }
  }
  
  # Formata a solução final, eliminando duplicatas
  rownames(final) <- c("tempo","distancia")
  final <- data.frame(t(final))
  final <- distinct(final, tempo, distancia, .keep_all = TRUE)
  final <- t(as.matrix(final))
  
  return(final)
}