###################################################################
# UNIVERSIDADE FEDERAL DE MINAS GERAIS                            
# BACHARELADO EM ENGENHARIA DE SISTEMAS                           
# DISCIPLINA: ELE088 Teoria da Decisao                            
# PROFESSOR: Lucas de Souza Batista                               
# ALUNOs: Ariel Domingues, Hernane Braga e Nikolas Fantoni        
# DATA: Novembro/2019                               
# TC3 - Tomada de Decisão Multicritério no PCV

# O algoritmo abaixo realiza o SA multiobjetivo utilizando a abordagem
# de Somas Ponderadas para uma dada solução inicial X

#Função de normalização
normalizando <- function(custo, mincusto, maxcusto){
  (custo-mincusto)/(maxcusto-mincusto)
}

# SA multiobjetivo PW
SAmultiSP <- function(X, dados_custo_tempo, dados_custo_distancia, wd, wt, maxit){
  
  ###################################################
  ###       BLOCO DAS CONSTANTES INICIAIS         ###
  ###################################################
  
  # Pré determina os custos mínimos com base no enunciado
  mint <- 16.5
  mind <- 1250
  
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
  
  # Calcula a temperatura inicial T0d e T0t com base na fórmula
  # e^-média(deltaE)/T0 = tau
  tau = 0.5
  deltaEt <- NULL
  deltaEd <- NULL
  for (i in 1:100){
    deltaEt <- c(deltaEt,(abs((sum(Vizinhanca(X, dados_custo_tempo,dados_custo_distancia, 1)$custotempo))) - (sum(X$custotempo))))
    deltaEd <- c(deltaEd,(abs((sum(Vizinhanca(X, dados_custo_tempo,dados_custo_distancia, 1)$custodistancia))) - (sum(X$custodistancia))))
  }
  T0t <- -mean(deltaEt)/(log(tau))
  T0d <- -mean(deltaEd)/(log(tau))
  rm(tau,i, deltaEt, deltaEd)

  ###################################################
  ###       BLOCO DO SIMULATED ANNEALING          ###
  ###################################################
  
  # Constantes do SA
  seqi <- seq(0.000001,1,0.000001) # Usada para sortear um valor
  iteracao <- 0 # Valor das iterações totais feitas
  nivel <- 1 # Nível de perturbação vizinhança inicial (quanto maior, maior a diferença entre as soluções)
  D0 <- 0.8 # Variação da temperatura inicial estática
  Tkt <- T0t # Próxima temperatura do tempo (começa já bem baixa)
  Tkd <- T0d # Próxima temperatura da distância (começa já bem baixa)
  m <- 1 # Valor das iterações que serão feitas para cada temperatura
  xbest <- X # Guarda a melhor solução
  costt <- NULL

  # Looping do SA multiobjetivo
  while (iteracao < maxit && Tkt > 0.000001*T0t && Tkd > 0.000001*T0d){
    aceitacao <- 0
    m <- 0
    menordeltaE <- Inf
    todosdeltaE <- NULL
    deltaEt <- NULL
    deltaEd <- NULL
   
    # Faz a repetição para cada mudança na temperatura
    while (m <= 20 && aceitacao <= 15){
      costold <- wd*normalizando(sum(X$custodistancia),mind, maxd)+wt*normalizando(sum(X$custotempo), mint, maxt) # Custo da solução atual
      x1 <- Vizinhanca(X, dados_custo_tempo,dados_custo_distancia, nivel) # Encontra nova solução na vizinhança
      costnew <- wd*normalizando(sum(x1$custodistancia),mind, maxd)+wt*normalizando(sum(x1$custotempo), mint, maxt) # Custo da nova solução
      deltaE <- costnew - costold # Calcula a diferença dos custos das soluções
      deltaEt <- sum(x1$custotempo)-sum(X$custotempo) # Calcula custo apenas em função do tempo
      deltaEd <- sum(x1$custodistancia)-sum(X$custodistancia) # Calcula custo apenas em função da distância
      
      # Caso a vizinhança retorne uma solução idêntica
      while (deltaE == 0){
        costold <- wd*normalizando(sum(X$custodistancia),mind, maxd)+wt*normalizando(sum(X$custotempo), mint, maxt)
        x1 <- Vizinhanca(X, dados_custo_tempo,dados_custo_distancia, nivel)
        costnew <- wd*normalizando(sum(x1$custodistancia),mind, maxd)+wt*normalizando(sum(x1$custotempo), mint, maxt)
        deltaE <- costnew - costold
        deltaEt <- sum(x1$custotempo)-sum(X$custotempo) # Calcula custo apenas em função do tempo
        deltaEd <- sum(x1$custodistancia)-sum(X$custodistancia) # Calcula custo apenas em função da distância
      }
      
      # Se a nova solução x1 for melhor que a anterior
      if (deltaE <= 0){
        if (deltaE < menordeltaE) {
          menordeltaE <- deltaE # Atualiza menor deltaE
        }
        todosdeltaE <- c(todosdeltaE,deltaE) # Guarda todos os deltaE de soluções aceita
        aceitacao <- aceitacao +1 # Atualiza o contador de soluções aceitas
        X <- x1 # Atualiza a solução atual
        costbest <- wd*normalizando(sum(xbest$custodistancia),mind, maxd)+wt*normalizando(sum(xbest$custotempo), mint, maxt)
        if (costnew < costbest) {
          xbest <- x1 #Atualiza a melhor solução
          Tkt <- T0t
          Tkd <- T0d
        
        }
        costt <- c(costt,costnew) # Atualiza os custos encontrados
      }
      # Se a nova solução x1 for pior que a anterior
      else {
        prob <- min(D0,exp(-deltaEd/Tkd))*min(D0,exp(-deltaEt/Tkt)) # Calcula a probabilidade da solução ser aceita
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
    
    # Atualiza as temperaturas com base na regra iterativa
    if (is.null(todosdeltaE)) {
      Tkt <- D0*Tkt
      Tkd <- D0*Tkd
    } else {
      Tkd <- min(abs(menordeltaE)/abs(mean(todosdeltaE)), D0)*Tkd
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
  
  # Retorna uma lista contendo o melhor resultado encontrado e os custos do algoritmo ao longo do SA
  return (xbest)
}