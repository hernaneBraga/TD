# Considerando os dados de custo como uma matriz 250x250 e a solucao como um data.frame 250x2
# onde as duas colunas se referem a 'destino' e 'custo', respectivamente. Ou seja, a linha x do data.frame 
# se refere a cidade x, sendo a primeira coluna a cidade destino e a segunda o custo de ir ate ela.

# Funcao de nivel de perturbacao 1. A cidade x tem como destino y, o que eh invertido. Ou seja, y passa
# a ter x como destino. As trocas subsequentes sao apenas para manter a solucao factivel.
TrocaSimples <- function(solucao_atual, dados_custo){
  nova_solucao <- solucao_atual
  cidade <- sample(dim(solucao_atual)[1], 1) # Escolhe-se uma cidade aleatoriamente
  
  # Para fazer a troca de forma a manter a solucao factivel eh preciso alterar as rotas de 4 cidades,
  # com 3 rotas sendo alteradas na pratica.
  vizinho_anterior <- which(solucao_atual$destino == cidade) 
  prox_vizinho1    <- solucao_atual$destino[cidade]
  prox_vizinho2    <- solucao_atual$destino[prox_vizinho1]
  
  # As trocas necessarias sao feitas com os novos custos extraidos da matriz de custos
  nova_solucao[prox_vizinho1,]    <- c(cidade, dados_custo[prox_vizinho1, cidade])
  nova_solucao[cidade,]           <- c(prox_vizinho2, dados_custo[cidade, prox_vizinho2])
  nova_solucao[vizinho_anterior,] <- c(prox_vizinho1, dados_custo[vizinho_anterior, prox_vizinho1])
  
  return(nova_solucao)
}

# Funcao que escolhera qual nivel de perturbacao utilizar. Para chama-la o parametro nivel deve ser
# passado entre aspas ('n1' por exemplo)
Vizinhanca <- function(solucao_atual, dados_custo, nivel){
  switch (nivel,
    n1 = TrocaSimples(solucao_atual, dados_custo)#,
    #n2 = vizinhanca2(solucao_atual, dados_custo),
    #n3 = vizinhanca3(solucao_atual, dados_custo),
    #n4 = vizinhanca4(solucao_atual, dados_custo),
    #n5 = vizinhanca5(solucao_atual, dados_custo),
    #n6 = vizinhanca6(solucao_atual, dados_custo)
  )
}
