###################################################################
# UNIVERSIDADE FEDERAL DE MINAS GERAIS                            
# BACHARELADO EM ENGENHARIA DE SISTEMAS                           
# DISCIPLINA: ELE088 Teoria da Decisao                            
# PROFESSOR: Lucas de Souza Batista                               
# ALUNOs: Ariel Domingues, Hernane Braga e Nikolas Fantoni        
# DATA: Outubro/2019                               
# TC1 - Otimizacao multi-objetivo do PCV


escolha_cidade <- function(matriz, r){
  
  dst_ordenado <- matriz[,order(matriz[2,])] 
  tempo_ordenado <- matriz[,order(matriz[3,])]
  
  
  m <- r
  if(i <= n-4){ idx <- round(runif(1,1,m)) }
  else if(i == n-3){ idx <- round(runif(1,1,2)) }
  else{ idx <- 1 }
  
  
  
  a <- dst_ordenado[which(dst_ordenado < Inf)]
  max_dst <- max(a)
  
  b <- tempo_ordenado[which(tempo_ordenado < Inf)]
  max_tempo <- max(b)
  
  if(is.infinite(dst_ordenado[2,idx]) == TRUE || is.infinite(tempo_ordenado[3,idx]) == TRUE ){ idx <- 1 }
  
  norm_dist <- (dst_ordenado[2, idx] - dst_ordenado[2,1])/(max_dst - dst_ordenado[2,1])
  norm_tempo <- (tempo_ordenado[3, idx] - tempo_ordenado[3,1])/(max_tempo - tempo_ordenado[3,1])

  
  if (norm_tempo <=  norm_dist  ){   return( tempo_ordenado[1:3,idx] ) }
  
  else { return(dst_ordenado[1:3,idx]) }
  
}


solucao_inicial <- function(nome_arquivo_dst, nome_arquivo_tempo, r){
# nome_arquivo: nome do .csv a ser lido e calculado
# r: numero de cidades vizinhas que sao candidatas a serem escolhidas. 
#    Quanto maior o valor r, maior a probabilidade da solucao inicial estar longe do ótimo
#    local gerado por uma solução totalmente gulosa.
     
# vetor_resultado: retorno da funcao com a solucao inicial - Retornado apenas linhas l2 e l3, ordenadas a partinha de l1
                   # Linha 1: Cidade
                   # Linha 2: Distancia ate cidade
                   # Linha 3: Tempo ate cidade

# Carregar dados
{

nome_arquivo_dst <- "distancia.csv" 
nome_arquivo_tempo <- "tempo.csv" 
  
dados_dst <- as.matrix(read.csv( nome_arquivo_dst, sep=',' ,header = FALSE ))
dados_tempo <- as.matrix(read.csv( nome_arquivo_tempo, sep=',' ,header = FALSE ))

# Define como infinito as cidades que vão para elas mesmas.
# Inf também sera usada para definir se uma cidade já foi visitada 
diag(dados_dst) <- Inf
diag(dados_tempo) <- Inf


}

# Caminho a partir da cidade 1
{
n <- length(dados_dst[,1])
cidade_atual <- 1        # Cidade inicial = 1
vetor_result <- matrix(0, nrow = 4, ncol = n)

vetor_aux <- matrix(0, nrow = 3, ncol = n)
vetor_aux[1,] <- seq(1, n, 1)  # indice das cidades
vetor_aux[2,] <- dados_dst[cidade_atual, ]   # custo de distancia
vetor_aux[3,] <- dados_tempo[cidade_atual, ] # custo de tempo

vetor_result[1,1] <- cidade_atual     # indice Cidade atual [i]

aux <- escolha_cidade(vetor_aux, r)
vetor_result[2:4,1] <- aux

cidade_atual <- aux[1]
dados_dst[,cidade_atual]   <- Inf # Marca a cidade escolhida como visitada
dados_tempo[,cidade_atual] <- Inf # Marca a cidade escolhida como visitada

}

# Calculo da solucao da 2a cidade ate n-1
vetor_aux <- matrix(0, nrow = 3, ncol = n-1) # Novo vetor auxiliar, excluindo a cidade 1 (inicial)
for (i in 2:(n-1)){

  vetor_aux[1,] <- seq(2, n, 1) #Preencendo a linha 1 com os indices das cidades
  vetor_aux[2,] <- dados_dst[cidade_atual, 2:n] #Preenchendo a linha 2 com as distancias das cidades
  vetor_aux[3,] <- dados_tempo[cidade_atual, 2:n] #Preenchendo a linha 2 com os tempos das cidades
  
  
  aux <- escolha_cidade(vetor_aux, r)
  
  vetor_result[1,i] <- cidade_atual
  vetor_result[2:4,i] <- aux
  
  
  cidade_atual <- aux[1]
  dados_dst[,cidade_atual] <- Inf # Marca a cidade escolhida como visitado
  dados_tempo[,cidade_atual] <- Inf # Marca a cidade escolhida como visitado  

}

# Calcula distancia minima para a cidade restante ( cidade inicial 1)
{

vetor_result[1,n] <- cidade_atual
  
vetor_aux <- matrix(0, nrow = 3, ncol = n)
vetor_aux[1,] <- seq(1, n, 1) 
vetor_aux[2,] <- dados_dst[cidade_atual, 1:n]
vetor_aux[3,] <- dados_tempo[cidade_atual, 1:n] 


aux <- escolha_cidade(vetor_aux, r)
cidade_atual <- aux[1]


vetor_result[2:4,n] <- aux



dados_dst[,cidade_atual] <- Inf # Marca a cidade escolhida como visitado
dados_tempo[,cidade_atual] <- Inf # Marca a cidade escolhida como visitado  


}

vetor_result <- vetor_result[,order(vetor_result[1,])] # Ordena matriz pela linha 1

# print(paste("Distancia = ", sum(vetor_result[3,])))
# print(paste("Tempo = ", sum(vetor_result[4,])))

return(vetor_result)

}
