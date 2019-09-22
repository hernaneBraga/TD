###################################################################
# UNIVERSIDADE FEDERAL DE MINAS GERAIS                            
# BACHARELADO EM ENGENHARIA DE SISTEMAS                           
# DISCIPLINA: ELE088 Teoria da Decisao                            
# PROFESSOR: Lucas de Souza Batista                               
# ALUNOs: Ariel Domingues, Hernane Braga e Nikolas Fantoni        
# DATA: Setembro/2019                               
# TC1 - Otimizacao mono-objetivo do PCV

# O algoritmo abaixo pode ser rodado como um todo ou em blocos, conforme dividido a seguir.
# É aconselhável a limpeza do ambiente antes da execução do algoritmo, usando a função abaixo.
rm(list=ls())

# Importando Bibliotecas, arquivos necessários e os dados
# Aqui deve ser definido se o arquivo distancia.csv ou o arquivo tempo.csv
# serão utilizados na otimizacao.
source('solucao_inicial.R')
source('vizinhanca.R')
dados_custo <- as.matrix(read.csv(file="distancia.csv", header=FALSE, sep=","))


###################################################
###           BLOCO DA SOLUÇÃO INICIAL          ###
###################################################

# Define o grau de variabilidade da solução inicial (número inteiro).
# Valores menores significam uma solução mais próxima da solução obtida
# a partir de um algoritmo guloso.

grau <- 1
X <- t(solucao_inicial("distancia.csv", grau))
colnames(X) <- c("destino", "custo")
X <- data.frame(X)
custoinicial <- sum(X$custo) #define custo inicial
# Para imprimir o custo inicial
# cat("Custo inicial: ", custoinicial, ".\n")

###################################################
###       BLOCO DA TEMPERATURA INICIAL          ###
###################################################

# Calcula a temperatura inicial T0 com base na fórmula
# e^-média(deltaE)/T0 = tau
tau <- 0.5
deltaE <- NULL
for (i in 1:100){
  deltaE <- c(deltaE,(abs((sum(Vizinhanca(X, dados_custo, 1)$custo))) - (sum(X$custo))))
}
T0 <- -mean(deltaE)/(log(tau))
rm(tau, i, grau, deltaE)

###################################################
###       BLOCO DO SIMULATED ANNEALING          ###
###################################################

# Constantes do SA
seqi <- seq(0.00001,1,0.00001) # Usada para sortear um valor
costt <- custoinicial # Guarda os custos ao longo do algoritmo
iteracao <- 0 # Valor das iterações totais feitas
nivel <- 1 # Nível de perturbação vizinhança inicial (quanto maior, maior a diferença entre as soluções)
D0 <- 0.5 # Variação da temperatura inicial estática
Tk <- T0 # Próxima temperatura
m <- 1 # Valor das iterações que serão feitas para cada temperatura
xbest <- X # Guarda a melhor solução

# Looping do SA
while (iteracao < 10000 && Tk > (0.00001*T0) && nivel<=7){
    aceitacao <- 0
    m <- 0
    menordeltaE <- Inf
    todosdeltaE <- NULL
    
    # Faz a repetição para cada mudança na temperatura
    while (m <= 400 && aceitacao <= 15){
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
      Tk <- min(abs(menordeltaE)/abs(mean(todosdeltaE)), D0)*Tk}

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

# Salva o custo final da melhor solução encontrada
custofinal <- sum(xbest$custo)

# Limpa variáveis não mais úteis
rm(cost1, cost2, prob, seqi)

###################################################
###             BLOCO DAS SOLUÇÕES              ###
###################################################

# Plota custos e imprime o custo final e o custo inicial
plot(1:length(costt), costt,type="l", xlab="",ylab="Custo", main="Evolução do custo ao longo do algoritmo SA")
cat("Custo inicial: ", custoinicial, ".\n")
cat("Custo final: ", custofinal,".\n")