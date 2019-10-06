solucao_inicial_gulosa <- function(nome_arquivo_distancia, nome_arquivo_tempo){

#Abre arquivo
distancias <- as.matrix(read.csv( nome_arquivo_distancia, sep=',' ,header = FALSE ))
tempo <- as.matrix(read.csv( nome_arquivo_tempo, sep=',' ,header = FALSE ))

#Solução inicial pelo algoritmo guloso
dados<-list() #Cria uma lista vazia
dados[[1]] <- sample(2:250,1) #Escolhe uma cidade aleatória para começar, tirando a 1
dados[[2]] <- as.numeric(tempo[1,dados[[1]]]) #Preenche o custo de ir de 1 para essa cidade
dados[[3]] <- as.numeric(distancias[1,dados[[1]]]) #Preenche o custo de ir de 1 para essa cidade
names(dados) <- c("destino", "custotempo", "custodistancia")

#Cria uma lista de cidades ainda não visitadas
listapossiveis <- 2:250
listapossiveis <- listapossiveis[-(which(listapossiveis==dados[[1]]))]
i <- dados[[1]] #Marca a primeira cidade visitada

#Algoritmo Guloso (vai procurar a melhor cidade da lista de possíveis e ir para ela)
for (j in 1:248){
  indice <- min(listapossiveis[which(as.numeric(distancias[i,listapossiveis]) == 
                                       min(as.numeric(distancias[i,listapossiveis])))]) #melhor cidade para ir
  dados[[1]][i] <- indice #adiciona cidade ao vetor de dados
  dados[[2]][i] <- tempo[i,indice] #adiciona a distancia para essa cidade
  dados[[3]][i] <- distancias[i,indice]
  listapossiveis <- listapossiveis[-which(listapossiveis==indice)] #atualiza a lista de possíveis cidades
  i <- indice #atualiza o indice
}
#Preenche a última cidade que faltou, que obrigatoriamente tem que voltar para a cidade 1
dados[[1]][indice] <- 1
dados[[2]][indice] <- as.numeric(tempo[indice,1])
dados[[3]][indice] <- as.numeric(distancias[indice,1])
#transforma os dados em um dataframe
dados <- data.frame(dados)

return(dados)
}