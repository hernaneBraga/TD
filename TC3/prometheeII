###############################################################################
# UNIVERSIDADE FEDERAL DE MINAS GERAIS
# BACHARELADO EM ENGENHARIA DE SISTEMAS
# DISCIPLINA: ELE088 Teoria da Decisao
# PROFESSOR: Lucas de Souza Batista
# ALUNOs: Ariel Domingues, Hernane Braga e Nikolas Fantoni
# DATA: Novembro/2019
# TC3 - Tomada de Decisão Multicritério no PCV

###############################################################################
# PROMETHEE II

###############################################################################
# Bloco de leitura das alternativas Pareto-otimas obtidos pelo SA multiobjetivo e
# conversao para os criterios escolhidos. 1- Menor distancia total; 2- Menor tempo
# total; 3- Menor desvio padrao das distancias; 4- Menor desvio padrao dos tempos
# 5- Tempo maximo entre cidades limitado a 1.3h

rm(list=ls())

solutions <- read.table("alternativas.txt")
num_alts <- length(solutions)/3
num_criteria <- 5
criteria <- data.frame(total_dist = numeric(num_alts),
                       total_time = numeric(num_alts),
                       dist_sd    = numeric(num_alts),
                       time_sd    = numeric(num_alts),
                       time_limit = numeric(num_alts))

alternatives <- list()
for (i in 0:(num_alts-1)) {
  alternatives[[i+1]] <- as.data.frame(solutions[,(i*3+1):(i*3+3)])
  colnames(alternatives[[i+1]]) <- c("destino", "tempo", "distancia")

  criteria$total_dist[i+1] <- sum(alternatives[[i+1]]$distancia)
  criteria$total_time[i+1] <- sum(alternatives[[i+1]]$tempo)
  criteria$dist_sd[i+1]    <- sd(alternatives[[i+1]]$distancia)
  criteria$time_sd[i+1]    <- sd(alternatives[[i+1]]$tempo)
  criteria$time_limit[i+1] <- max(1*(alternatives[[i+1]]$tempo > 1.3))
}

# Pesos obtidos atraves das prioridades geradas pelo AHP
w <- c(0.35225230, 0.35225230, 0.07323682, 0.07323682, 0.14902177)

# Funcao G de calculo da preferencia
G <- function(x){
  if(x <= 0)
    return(0)
  return(1)
}

P <- matrix(0, num_alts, num_alts)
Pj <- list()
for (j in 1:num_criteria) {
  Pj[[j]] <- matrix(0, num_alts, num_alts)
  for (i in 1:num_alts) {
    for (k in 1:num_alts) {
      Pj[[j]][i,k] <- G(-(criteria[[j]][i] - criteria[[j]][k]))
      P[i,k] <- P[i,k] + w[j]*Pj[[j]][i,k]
    }
  }
}

P <- P/sum(w)

fluxo <- numeric(num_alts)
for (i in 1:num_alts) {
  fluxo[i] <- sum(P[i,]) - sum(P[,i])
}

sobreclassificacao <- matrix(0, num_alts, num_alts)
for (i in 1:num_alts) {
  for (k in 1:num_alts) {
    sobreclassificacao[i,k] <- 1*(fluxo[i] > fluxo[k])
  }
}

# Matriz de sobreclassificacao
sobreclassificacao

# Ordem de escolha encontrada para as alternativas  
order_of_preference <- order(-rowSums(sobreclassificacao))

cat("\nThe best alternative in this configuration is: ", order_of_preference[1],
    "\nThe order of preference: ", order_of_preference)
