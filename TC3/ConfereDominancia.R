###################################################################
# UNIVERSIDADE FEDERAL DE MINAS GERAIS                            
# BACHARELADO EM ENGENHARIA DE SISTEMAS                           
# DISCIPLINA: ELE088 Teoria da Decisao                            
# PROFESSOR: Lucas de Souza Batista                               
# ALUNOs: Ariel Domingues, Hernane Braga e Nikolas Fantoni        
# DATA: Novembro/2019                               
# TC3 - Tomada de Decisão Multicritério no PCV

# O algoritmo abaixo Confere se as soluções encontradas são Pareto-Ótimas
# excluindo as soluções que possuem relação de dominância


# Compara todas as soluções, uma a uma, para ver se há dominância
ConfereDominancia <- function(solucoes){
  # Formata a solução inicial, eliminando duplicatas
  for (kk in 1:length(solucoes)){
    colnames(solucoes[[kk]]) <- c("destino","tempo","distancia")
  }
  solucoes <- unique(solucoes)
  final <- list()
  final[[1]] <- solucoes[[1]]
  if (length(solucoes)==1){
    return (final)
  } else {
    for (ii in 2:length(solucoes)){
      
      breakpoint <- length(final)
      jj <- 1
      candidata <- 0
      while (jj <= breakpoint){
        if((sum(solucoes[[ii]]$tempo) > sum(final[[jj]]$tempo)) && (sum(solucoes[[ii]]$distancia) < sum(final[[jj]]$distancia)) ||
           (sum(solucoes[[ii]]$tempo) < sum(final[[jj]]$tempo)) && (sum(solucoes[[ii]]$distancia) > sum(final[[jj]]$distancia))){
          candidata <- candidata+1
        } 
        if((sum(solucoes[[ii]]$tempo) <= sum(final[[jj]]$tempo)) && (sum(solucoes[[ii]]$distancia) <= sum(final[[jj]]$distancia))){
          final[[jj]] <- solucoes[[ii]] 
        }
        final <- unique(final)
        breakpoint <- length(final)
        jj <- jj+1
      }
      if (candidata == length(final)){
        final[[length(final)+1]] <- solucoes[[ii]]
      }
      final <- unique(final)
    }
  }
  final <- unique(final)
  return(final)
}