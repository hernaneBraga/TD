###############################################################################
# UNIVERSIDADE FEDERAL DE MINAS GERAIS
# BACHARELADO EM ENGENHARIA DE SISTEMAS
# DISCIPLINA: ELE088 Teoria da Decisao
# PROFESSOR: Lucas de Souza Batista
# ALUNOs: Ariel Domingues, Hernane Braga e Nikolas Fantoni
# DATA: Novembro/2019
# TC3 - Tomada de Decisão Multicritério no PCV

###############################################################################
# ELECTRE I

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

#Normalizando os critérios de 0 a 1 (0 pior, 1 melhor)
criteria_norm <- criteria
for (j in 1:num_criteria){
  best_alt  <- min(criteria[[j]])
  worst_alt <- max(criteria[[j]])
  if(worst_alt != best_alt){
    criteria_norm[[j]] <- (worst_alt - criteria[[j]])/(worst_alt - best_alt)
  }else{
    criteria_norm[[j]] <- criteria[[j]]*0
  }

}

Pplus  <- matrix(0,num_alts,num_alts)
Pequal <- matrix(0,num_alts,num_alts)
for (j in 1:num_criteria) {
  for (i in 1:num_alts) {
    for (k in 1:num_alts) {
      if (i != k) {
        Pplus[i,k]  <- Pplus[i,k] + w[j]*(criteria_norm[[j]][i] > criteria_norm[[j]][k])
        Pequal[i,k] <- Pequal[i,k] + w[j]*(criteria_norm[[j]][i] == criteria_norm[[j]][k])
      }
    }
  }
}

Concordancia <- (Pplus+Pequal)/sum(w)

Discordancia <- matrix(0,num_alts,num_alts)

for (i in 1:num_alts) {
  for (k in 1:num_alts) {
    Jminusik    <- NULL
    difJminusik <- NULL
    if (i != k) {
      for (j in 1:num_criteria) {
        Jminusik    <- c(Jminusik, (criteria_norm[[j]][i] < criteria_norm[[j]][k]))
        difJminusik <- c(difJminusik, (criteria_norm[[j]][k] - criteria_norm[[j]][i]))
      }
      Discordancia[i,k] <- max(Jminusik*difJminusik)
    }
  }
}

Sobreclassificacao <- matrix(0,num_alts,num_alts)
for (i in 1:num_alts){
  for (k in 1:num_alts){
    if (i!=k){
      if (Concordancia[i,k] >= 0.60 && Discordancia[i,k] <= 0.40){
        Sobreclassificacao[i,k]<-1
      }
    }
  }
}

# Matriz de sobreclassificacao
Sobreclassificacao

# # Pseudo-ordem de classificacao (algumas alternativas podem ser incomparaveis)
#order(-(rowSums(Sobreclassificacao) - colSums(Sobreclassificacao)))
