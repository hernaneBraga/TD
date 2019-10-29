###############################################################################
# UNIVERSIDADE FEDERAL DE MINAS GERAIS                            
# BACHARELADO EM ENGENHARIA DE SISTEMAS                           
# DISCIPLINA: ELE088 Teoria da Decisao                            
# PROFESSOR: Lucas de Souza Batista                               
# ALUNOs: Ariel Domingues, Hernane Braga e Nikolas Fantoni        
# DATA: Novembro/2019                               
# TC3 - Tomada de Decisão Multicritério no PCV

###############################################################################
# Bloco de leitura das alternativas Pareto-otimas obtidos pelo SA multiobjetivo e
# conversao para os criterios escolhidos. 1- Menor distancia total; 2- Menor tempo 
# total; 3- Menor desvio padrao das distancias; 4- Menor desvio padrao dos tempos
# 5- Tempo maximo entre cidades limitado a 1.3h

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
  criteria$time_limit[i+1] <- sum(alternatives[[i+1]]$tempo > 1.3)
}

###############################################################################
# Bloco de definicao das relacoes entre os criterios escolhidos

criteria_matrix <- t(matrix(c(1,    1,    4,   4,   6,
                              1,    1,    4,   4,   6,
                              0.25, 0.25, 1,   1,   3,
                              0.25, 0.25, 1,   1,   3,
                              0.15, 0.15, 0.4, 0.4, 1), num_criteria, num_criteria))

colnames(criteria_matrix) <- c("total_dist", "total_time", "dist_sd", "time_sd", "time_limit")
rownames(criteria_matrix) <- c("total_dist", "total_time", "dist_sd", "time_sd", "time_limit")

lambda_max <- as.numeric(eigen(criteria_matrix)$values[1]) # O primeiro eh o autovalor maximo
criteria_inconsistency <- (lambda_max - num_criteria)/(num_criteria - 1) # Inconsistencia da matriz de criterios
if(criteria_inconsistency > 0.1){
  stop("High inconsistency in the criteria matrix. Please change the relations.")
}

eigenvector <- as.numeric(eigen(criteria_matrix)$vectors[,1]) # Autovetor direito principal
criteria_matrix <- cbind(criteria_matrix, eigenvector/sum(eigenvector)) # Prioridades dos criterios (autovetor normalizados)
colnames(criteria_matrix)[num_criteria+1] <- "prioridade"

###############################################################################
# Bloco de definicao das relacoes entre as alternativas dados os criterios
# Todos os criterios buscam o minimo possivel. Podemos analisar a ordem e normalizar a nota entre 1 e 9, 
# sendo 1 para a alternativa com o menor valor do criterio e 9 para a com o maior.

alternatives_matrix <- list()
alternatives_inconsistency <- numeric(num_criteria)
for (i in 1:num_criteria) {
  ordem <- order(criteria[[i]])
  best_alt  <- criteria[[i]][ordem[1]]
  worst_alt <- criteria[[i]][ordem[num_alts]]
  alternatives_norm <- 1 + 8*(criteria[[i]] - best_alt)/(worst_alt - best_alt)
  alternatives_matrix[[i]] <- matrix(0, num_alts, num_alts)
  for (j in 1:num_alts) {
    alternatives_matrix[[i]][j,] <- alternatives_norm/alternatives_norm[j]
  }
  
  lambda_max <- as.numeric(eigen(alternatives_matrix[[i]])$values[1]) # O primeiro eh o autovalor maximo
  alternatives_inconsistency[i] <- (lambda_max - num_alts)/(num_alts - 1)# Inconsistencia das matrizes de alternativas
  
  eigenvector <- as.numeric(eigen(alternatives_matrix[[i]])$vectors[,1])
  alternatives_matrix[[i]] <- cbind(alternatives_matrix[[i]], eigenvector/sum(eigenvector)) # Prioridades das alternativas
  colnames(alternatives_matrix[[i]]) <- c(1:num_alts, "prioridade")
  rownames(alternatives_matrix[[i]]) <- c(1:num_alts)
}

###############################################################################
# Calculo das relacoes finais de prioridade

preferences <- numeric(num_alts)
for (i in 1:num_criteria) {
  preferences <- preferences + t(alternatives_matrix[[i]][,(num_alts+1)])*criteria_matrix[i,(num_criteria+1)]
}

order_of_preference <- order(-preferences)

cat("\nThe best alternative in this configuration is: ", order_of_preference[1],
    "\nThe order of preference: ", order_of_preference)
