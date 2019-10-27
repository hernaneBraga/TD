###################################################################
# UNIVERSIDADE FEDERAL DE MINAS GERAIS                            
# BACHARELADO EM ENGENHARIA DE SISTEMAS                           
# DISCIPLINA: ELE088 Teoria da Decisao                            
# PROFESSOR: Lucas de Souza Batista                               
# ALUNOs: Ariel Domingues, Hernane Braga e Nikolas Fantoni        
# DATA: Novembro/2019                               
# TC3 - Tomada de Decisão Multicritério no PCV

EncontraSolucoes <- function(solInicial,dados_custo_tempo, dados_custo_distancia,maxit){
  
    # Realiza o SA no método Pw para vários pesos
    seqj <- seq(0.02,0.98,0.02)
    candidatas <- list()
    contador <- 1
    for (j in seqj){
      candidatas[[contador]] <- SAmultiSP(solInicial,dados_custo_tempo, dados_custo_distancia,wd=j,wt = (1-j), maxit = maxit)
      contador <- contador+1
    }
    saida <- ConfereDominancia(candidatas)
    return(saida)
}