###################################################################
# UNIVERSIDADE FEDERAL DE MINAS GERAIS                            
# BACHARELADO EM ENGENHARIA DE SISTEMAS                           
# DISCIPLINA: ELE088 Teoria da Decisao                            
# PROFESSOR: Lucas de Souza Batista                               
# ALUNOs: Ariel Domingues, Hernane Braga e Nikolas Fantoni        
# DATA: Novembro/2019                               
# TC3 - Tomada de Decisão Multicritério no PCV


#Electree I com os critérios:
#c1 = menor distância total
#c2 = menor tempo total
#c3 = menor variação da distancia média
#c4 = menor variação do tempo médio
#c5 = tempo máximo entre as cidades de 1,3h

#electree <- function(solucoes){
  
  #Pesos para o Electree
  w1 <- 0.2
  w2 <- 0.2
  w3 <- 0.4
  w4 <- 0.4
  w5 <- 0.5
  delta <- 20
  
  
  #Cálculo de cada critério
  c1 <- NULL
  c2 <- NULL
  c3 <- NULL
  c4 <- NULL
  c5 <- NULL
  for (i in 1:length(solucoes)){
    c1 <- c(c1,sum(solucoes[[i]]$distancia))
    c2 <- c(c2,sum(solucoes[[i]]$tempo))
    c3 <- c(c3,sd(solucoes[[i]]$distancia))
    c4 <- c(c4,sd(solucoes[[i]]$tempo))
    if (unique(max(solucoes[[i]]$tempo))<= 1.3){
      c5 <- c(c5,20)
    } else{
      c5 <- c(c5,0)
    }
  }
  
  #Normalizando os critérios de [0:delta]
  c1min <- unique(min(c1))
  c1max <- unique(max(c1))
  c2min <- unique(min(c2))
  c2max <- unique(max(c2))
  c3min <- unique(min(c3))
  c3max <- unique(max(c3))
  c4min <- unique(min(c4))
  c4max <- unique(max(c4))
  c5min <- unique(min(c5))
  c5max <- unique(max(c5))
  for (i in 1:length(solucoes)){
    c1[i] <- delta*(c1[i]-c1min)/(c1max-c1min)
    c2[i] <- delta*(c2[i]-c2min)/(c2max-c2min)
    c3[i] <- delta*(c3[i]-c3min)/(c3max-c3min)
    c4[i] <- delta*(c4[i]-c4min)/(c4max-c4min)
    c5[i] <- delta*(c5[i]-c5min)/(c5max-c5min)
  }
  rm(c1min,c1max,c2min,c2max,c3min,c3max,c4min,c4max)
  
  Jplus <- matrix(0,ncol=length(solucoes),nrow=length(solucoes))
  for (i in 1:length(solucoes)){
    for (j in 1:length(solucoes)){
      if (i != j){
       Jplus[i,j] <- w1*(c1[i] > c1[j])+w2*(c2[i] > c2[j])+
          w3*(c3[i] > c3[j])+w4*(c4[i] > c4[j]+w5*(c5[i] > c5[j]))
      }
    }
  }
  Jequal <- matrix(0,ncol=length(solucoes),nrow=length(solucoes))
  for (i in 1:length(solucoes)){
    for (j in 1:length(solucoes)){
      if (i != j){
        Jequal[i,j] <- w1*(c1[i] == c1[j])+w2*(c2[i] == c2[j])+
          w3*(c3[i] == c3[j])+w4*(c4[i] == c4[j])+ w5*(c5[i] == c5[j])
      }
    }
  }
  Jminus <- matrix(0,ncol=length(solucoes),nrow=length(solucoes))
  for (i in 1:length(solucoes)){
    for (j in 1:length(solucoes)){
      if (i != j){
        Jminus[i,j] <- w1*(c1[i] < c1[j])+w2*(c2[i] < c2[j])+
          w3*(c3[i] < c3[j])+w4*(c4[i] < c4[j])+w5*(c5[i] < c5[j])
      }
    }
  }
  Concordancia <- Jplus+Jequal/(w1+w2+w3+w4)
  Discordancia <- matrix(0,ncol=length(solucoes),nrow=length(solucoes))
  for (i in 1:length(solucoes)){
    for (j in 1:length(solucoes)){
      if (i != j){
        aux <- c((c1[i] < c1[j]),(c2[i] < c2[j]),(c3[i] < c3[j]),(c4[i] < c4[j]),(c5[i] < c5[j]))
        aux2 <- c(c1[j]-c1[i],c2[j]-c2[i],c3[j]-c3[i],c4[j]-c4[i], c5[j]-c5[i])
        Discordancia[i,j] <- (1/delta)*max(aux2*aux)
      }
    }
  }
  
  Sobreclassificacao <- matrix(0,ncol=length(solucoes),nrow=length(solucoes))
  for (i in 1:length(solucoes)){
    for (j in 1:length(solucoes)){
      if (i!=j){
        if (Concordancia[i,j] >= 0.70 && Discordancia[i,j] <= 0.30){
          Sobreclassificacao[i,j]<-1
        }
      }
    }
  }
  Sobreclassificacao
#}