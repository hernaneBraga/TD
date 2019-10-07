###################################################################
# UNIVERSIDADE FEDERAL DE MINAS GERAIS                            
# BACHARELADO EM ENGENHARIA DE SISTEMAS                           
# DISCIPLINA: ELE088 Teoria da Decisao                            
# PROFESSOR: Lucas de Souza Batista                               
# ALUNOs: Ariel Domingues, Hernane Braga e Nikolas Fantoni        
# DATA: Outubro/2019                               
# TC1 - Otimizacao multi-objetivo do PCV

# O algoritmo abaixo realiza o SA multiobjetivo para uma dada solução inicial X

#Função de normalização
normalizando <- function(custo, mincusto, maxcusto){
  (custo-mincusto)/(maxcusto-mincusto)
}

#SA multiobjetivo Epsilon-Restrito
SAmultiER <- function(X, dados_custo_tempo, dados_custo_distancia, epsilon, maxit){
  #Define os custos máximos usando uma solucão inicial aleatória
  Xrand <- sample(2:250)
  maxd <- dados_custo_distancia[1,Xrand[1]] 
  maxt <- dados_custo_tempo[1,Xrand[1]]
  for (i in 1:(length(Xrand)-1)){
    maxd <- c(maxd,dados_custo_distancia[Xrand[i],Xrand[i+1]])
    maxt <- c(maxt,dados_custo_tempo[Xrand[i],Xrand[i+1]])
  }
  maxd <- c(maxd,dados_custo_distancia[Xrand[i+1],1])
  maxt <- c(maxt,dados_custo_tempo[Xrand[i+1],1])
  destino <- c(Xrand,1)
  maxd <- sum(maxd)
  maxt <- sum(maxt)
  
  tipo <- 0 # Tipo 1 é de distância, tipo 2 é de tempo
  # Define se vai otimizar tempo ou distância pelo epsilon
  if (abs(epsilon-maxd) >= abs(epsilon-maxt)) tipo <- 1
  else tipo <- 2
  
  # Seleciona o tipo de otimização
  switch(tipo,
         SAD(X,dados_custo_tempo,dados_custo_distancia,epsilon,maxit),
         SAT(X,dados_custo_tempo,dados_custo_distancia,epsilon,maxit)
         )
}

# SA otimizando distância
SAD <- function(X, dados_custo_tempo, dados_custo_distancia, epsilon, maxit){
  # Calcula a temperatura inicial T0d e T0t com base na fórmula
  # e^-média(deltaE)/T0 = tau
  tau = 0.5
  deltaEd <- NULL
  for (i in 1:100){
    deltaEd <- c(deltaEd,(abs((sum(Vizinhanca(X, dados_custo_tempo,dados_custo_distancia, 1)$custodistancia))) - (sum(X$custodistancia))))
  }
  T0d <- -mean(deltaEd)/(log(tau))
  rm(tau,i, deltaEd)
  
  ###################################################
  ###       BLOCO DO SIMULATED ANNEALING          ###
  ###################################################
  
  # Constantes do SA
  seqi <- seq(0.00001,1,0.00001) # Usada para sortear um valor
  iteracao <- 0 # Valor das iterações totais feitas
  nivel <- 1 # Nível de perturbação vizinhança inicial (quanto maior, maior a diferença entre as soluções)
  D0 <- 0.9 # Variação da temperatura inicial estática
  Tkd <- T0d # Próxima temperatura da distância 
  m <- 1 # Valor das iterações que serão feitas para cada temperatura
  xbest <- X # Guarda a melhor solução
  costt <- NULL
  
  # Looping do SA multiobjetivo
  while (iteracao < maxit && Tkd > 0.0000001*T0d){
    aceitacao <- 0
    m <- 0
    menordeltaE <- Inf
    todosdeltaE <- NULL
    deltaEd <- NULL
    
    # Faz a repetição para cada mudança na temperatura
    while (m <= 20 && aceitacao <= 15){
      costold <- sum(X$custodistancia) # Custo da solução atual
      x1 <- Vizinhanca(X, dados_custo_tempo,dados_custo_distancia, nivel) # Encontra nova solução na vizinhança
      costnew <- sum(x1$custodistancia) # Custo da nova solução
      deltaE <- costnew - costold # Calcula a diferença dos custos das soluções
      deltaEd <- sum(x1$custodistancia)-sum(X$custodistancia) # Calcula custo apenas em função da distância
      
      # Caso a vizinhança retorne uma solução idêntica
      while (deltaE == 0){
        costold <- sum(X$custodistancia) # Custo da solução atual
        x1 <- Vizinhanca(X, dados_custo_tempo,dados_custo_distancia, nivel) # Encontra nova solução na vizinhança
        costnew <- sum(x1$custodistancia) # Custo da nova solução
        deltaE <- costnew - costold # Calcula a diferença dos custos das soluções
        deltaEd <- sum(x1$custodistancia)-sum(X$custodistancia) # Calcula custo apenas em função da distância
      }
      
      # Se a nova solução x1 for melhor que a anterior
      if (deltaE <= 0){
        if (x1$custotempo <= epsilon){
        if (deltaE < menordeltaE) {
          menordeltaE <- deltaE # Atualiza menor deltaE
        }
        todosdeltaE <- c(todosdeltaE,deltaE) # Guarda todos os deltaE de soluções aceita
        aceitacao <- aceitacao +1 # Atualiza o contador de soluções aceitas
        X <- x1 # Atualiza a solução atual
        costbest <- sum(xbest$custodistancia)
        if (costnew < costbest) {
          xbest <- x1 #Atualiza a melhor solução
          Tkd <- T0d*0.001
          
        }
        costt <- c(costt,costnew) # Atualiza os custos encontrados
        }
      }
      # Se a nova solução x1 for pior que a anterior
      else {
        prob <- min(D0,exp(-deltaEd/Tkd)) # Calcula a probabilidade da solução ser aceita
        if (is.na(prob)) prob <- 0
        if (sample(seqi,1)<prob) { # Se for aceita, parte análoga ao caso de x1 ser melhor que X
          if (deltaE < menordeltaE) {
            menordeltaE <- deltaE
          }
          todosdeltaE <- c(todosdeltaE,deltaE)
          aceitacao <- aceitacao +1
          X <- x1
          costt <- c(costt,costnew)
        }
      }
      m <- m+1 # Contador de iterações por temperatura
    }
    
    # Atualiza a temperatura com base na regra iterativa
    if (is.null(todosdeltaE)) {
      Tkd <- D0*Tkd
    } else {
      Tkd <- min(abs(menordeltaE)/abs(mean(todosdeltaE)), D0)*Tkd
    }
    
    # Se não foram aceitas nenhuma solução, aumenta o nível da vizinhança
    if (aceitacao<1) {
      if (nivel < 6){
        nivel <- nivel+1
      } 
    }
    # Se foram aceitas quaisquer soluções, volta para a vizinhança de nível mais baixo
    # para tentar uma busca local
    if (aceitacao >= 1){
      if (nivel > 1){
        nivel <- 1
      } 
    }
    
    iteracao <- iteracao + 1 # Incrementa a iteração total do algoritmo
    # Acompanha iteração no console (pra ver se não caiu num looping infinito)
    cat("iteracao: ", iteracao, "\n")
  }
  
  return (list(xbest, costt))
}

# SA otimizando tempo
SAt <- function(X, dados_custo_tempo, dados_custo_distancia, epsilon, maxit){
  # Calcula a temperatura inicial T0d e T0t com base na fórmula
  # e^-média(deltaE)/T0 = tau
  tau = 0.5
  deltaEt <- NULL
  for (i in 1:100){
    deltaEt <- c(deltaEt,(abs((sum(Vizinhanca(X, dados_custo_tempo,dados_custo_distancia, 1)$custotempo))) - (sum(X$custotempo))))
  }
  T0t <- -mean(deltaEt)/(log(tau))
  rm(tau,i, deltaEt)
  
  ###################################################
  ###       BLOCO DO SIMULATED ANNEALING          ###
  ###################################################
  
  # Constantes do SA
  seqi <- seq(0.00001,1,0.00001) # Usada para sortear um valor
  iteracao <- 0 # Valor das iterações totais feitas
  nivel <- 1 # Nível de perturbação vizinhança inicial (quanto maior, maior a diferença entre as soluções)
  D0 <- 0.9 # Variação da temperatura inicial estática
  Tkt <- T0t # Próxima temperatura da distância 
  m <- 1 # Valor das iterações que serão feitas para cada temperatura
  xbest <- X # Guarda a melhor solução
  costt <- NULL
  
  # Looping do SA multiobjetivo
  while (iteracao < maxit && Tkt > 0.0000001*T0t){
    aceitacao <- 0
    m <- 0
    menordeltaE <- Inf
    todosdeltaE <- NULL
    deltaEt <- NULL
    
    # Faz a repetição para cada mudança na temperatura
    while (m <= 20 && aceitacao <= 15){
      costold <- sum(X$custotempo) # Custo da solução atual
      x1 <- Vizinhanca(X, dados_custo_tempo,dados_custo_ditancia, nivel) # Encontra nova solução na vizinhança
      costnew <- sum(x1$custotempo) # Custo da nova solução
      deltaE <- costnew - costold # Calcula a diferença dos custos das soluções
      deltaEt <- sum(x1$custotempo)-sum(X$custotempo) # Calcula custo apenas em função do tempo
      
      # Caso a vizinhança retorne uma solução idêntica
      while (deltaE == 0){
        costold <- sum(X$custotempo) # Custo da solução atual
        x1 <- Vizinhanca(X, dados_custo_tempo,dados_custo_distancia, nivel) # Encontra nova solução na vizinhança
        costnew <- sum(x1$custotempo) # Custo da nova solução
        deltaE <- costnew - costold # Calcula a diferença dos custos das soluções
        deltaEt <- sum(x1$custotempo)-sum(X$custotempo) # Calcula custo apenas em função do tempo
      }
      
      # Se a nova solução x1 for melhor que a anterior
      if (deltaE <= 0){
        if (x1$custodistancia <= epsilon){
          if (deltaE < menordeltaE) {
            menordeltaE <- deltaE # Atualiza menor deltaE
          }
          todosdeltaE <- c(todosdeltaE,deltaE) # Guarda todos os deltaE de soluções aceita
          aceitacao <- aceitacao +1 # Atualiza o contador de soluções aceitas
          X <- x1 # Atualiza a solução atual
          costbest <- sum(xbest$custotempo)
          if (costnew < costbest) {
            xbest <- x1 #Atualiza a melhor solução
            Tkt <- T0t*0.001
            
          }
          costt <- c(costt,costnew) # Atualiza os custos encontrados
        }
      }
      # Se a nova solução x1 for pior que a anterior
      else {
        prob <- min(D0,exp(-deltaEt/Tkt)) # Calcula a probabilidade da solução ser aceita
        if (is.na(prob)) prob <- 0
        if (sample(seqi,1)<prob) { # Se for aceita, parte análoga ao caso de x1 ser melhor que X
          if (deltaE < menordeltaE) {
            menordeltaE <- deltaE
          }
          todosdeltaE <- c(todosdeltaE,deltaE)
          aceitacao <- aceitacao +1
          X <- x1
          costt <- c(costt,costnew)
        }
      }
      m <- m+1 # Contador de iterações por temperatura
    }
    
    # Atualiza a temperatura com base na regra iterativa
    if (is.null(todosdeltaE)) {
      Tkt <- D0*Tkt
    } else {
      Tkt <- min(abs(menordeltaE)/abs(mean(todosdeltaE)), D0)*Tkt
    }
    
    # Se não foram aceitas nenhuma solução, aumenta o nível da vizinhança
    if (aceitacao<1) {
      if (nivel < 6){
        nivel <- nivel+1
      } 
    }
    # Se foram aceitas quaisquer soluções, volta para a vizinhança de nível mais baixo
    # para tentar uma busca local
    if (aceitacao >= 1){
      if (nivel > 1){
        nivel <- 1
      } 
    }
    
    iteracao <- iteracao + 1 # Incrementa a iteração total do algoritmo
  }
  
  return (list(xbest, costt))
}
