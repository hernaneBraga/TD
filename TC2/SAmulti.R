###################################################################
# UNIVERSIDADE FEDERAL DE MINAS GERAIS                            
# BACHARELADO EM ENGENHARIA DE SISTEMAS                           
# DISCIPLINA: ELE088 Teoria da Decisao                            
# PROFESSOR: Lucas de Souza Batista                               
# ALUNOs: Ariel Domingues, Hernane Braga e Nikolas Fantoni        
# DATA: Outubro/2019                               
# TC1 - Otimizacao multi-objetivo do PCV

# O algoritmo abaixo realiza o SA multiobjetivo para uma dada solução inicial X

SAmulti <- function(X, dados_custo_tempo, dados_custo_distancia, w1, w2){
  
  # Calcula a temperatura inicial T0d e T0t com base na fórmula
  # e^-média(deltaE)/T0 = tau
  tau = 0.5
  deltaEt <- NULL
  deltaEd <- NULL
  for (i in 1:100){
    deltaEt <- c(deltaEt,(abs((sum(Vizinhanca(X, dados_custo_tempo, 1)$custo))) - (sum(X$custo))))
    deltaEd <- c(deltaEd,(abs((sum(Vizinhanca(X, dados_custo_distancia, 1)$custo))) - (sum(X$custo))))
  }
  T0t <- -mean(deltaEt)/(log(tau))
  T0d <- -mean(deltaEd)/(log(tau))
  rm(i, deltaEt, deltaEd)
  
  ###################################################
  ###       BLOCO DO SIMULATED ANNEALING          ###
  ###################################################
  
  # Constantes do SA
  seqi <- seq(0.00001,1,0.00001) # Usada para sortear um valor
  costt <- custoinicial # Guarda os custos ao longo do algoritmo
  iteracao <- 0 # Valor das iterações totais feitas
  nivel <- 1 # Nível de perturbação vizinhança inicial (quanto maior, maior a diferença entre as soluções)
  D0 <- 0.7 # Variação da temperatura inicial estática
  Tkt <- T0t # Próxima temperatura do tempo
  Tkd <- T0d # Próxima temperatura da distância
  m <- 1 # Valor das iterações que serão feitas para cada temperatura
  xbest <- X # Guarda a melhor solução
  
  # Looping do SA multiobjetivo
  while (iteracao < 1000 && Tkt > (0.00001*T0t) && Tkd > (0.00001*T0d) && nivel<=6){
    aceitacao <- 0
    m <- 0
    menordeltaE <- Inf
    todosdeltaE <- NULL
    
    # Faz a repetição para cada mudança na temperatura
    while (m <= 400 && aceitacao <= 20){
      cost1 <- sum(X$custo) # Custo da solução atual
      x1 <- Vizinhanca(X, dados_custo, nivel) # Encontra nova solução na vizinhança
      cost2 <- sum(x1$custo) # Custo da nova solução
      deltaE <- cost2 - cost1 # Calcula a diferença dos custos das soluções
      
      # Caso a vizinhança retorne uma solução idêntica
      while (deltaE == 0){
        cost1 <- sum(X$custo)
        x1 <- Vizinhanca(X, dados_custo, nivel)
        cost2 <- sum(x1$custo)
        deltaE <- cost2 - cost1
      }
      
      # Se a nova solução x1 for melhor que a anterior
      if (deltaE <= 0){
        if (deltaE < menordeltaE) {
          menordeltaE <- deltaE # Atualiza menor deltaE
        }
        todosdeltaE <- c(todosdeltaE,deltaE) # Guarda todos os deltaE de soluções aceita
        aceitacao <- aceitacao +1 # Atualiza o contador de soluções aceitas
        X <- x1 # Atualiza a solução atual
        if (sum(x1$custo) < sum(xbest$custo)) xbest <- x1 #Atualiza a melhor solução
        costt <- c(costt,cost2) # Atualiza os custos encontrados
      }
      # Se a nova solução x1 for pior que a anterior
      else {
        prob <- exp(-deltaE/Tk) # Calcula a probabilidade da solução ser aceita
        if (sample(seqi,1)<prob) { # Se for aceita, parte análoga ao caso de x1 ser melhor que X
          if (deltaE < menordeltaE) {
            menordeltaE <- deltaE
          }
          todosdeltaE <- c(todosdeltaE,deltaE)
          aceitacao <- aceitacao +1
          X <- x1
          costt <- c(costt,cost2)
        }
      }
      m <- m+1 # Contador de iterações por temperatura
    }
    
    # Atualiza a temperatura com base na regra iterativa
    if (is.null(todosdeltaE)) {
      Tk <- D0*Tk
    } else {
      Tk <- min(abs(menordeltaE)/abs(mean(todosdeltaE)), D0)*Tk
    }
    
    # Se não foram aceitas nenhuma solução, aumenta o nível da vizinhança
    if (aceitacao<1) {
      nivel <- nivel + 1
    }
    # Se foram aceitas quaisquer soluções, volta para a vizinhança de nível mais baixo
    # para tentar uma busca local
    if (aceitacao >= 1){
      nivel <- 1
    }
    
    iteracao <- iteracao + 1 # Incrementa a iteração total do algoritmo
  }
  
  return (xbest)
}