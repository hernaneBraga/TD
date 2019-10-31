source('ConfereDominancia.R')

#lendo alternativas
mydata <- read.table("alternativas.txt")
alternativas <- list()
for (i in 0:(length(mydata)/3-1)){
  aux <- mydata[(1+3*i):(3+3*i)]
  colnames(aux) <- c("destino","tempo","distancia")
  alternativas[[i+1]] <-aux
}

alternativas <- ConfereDominancia(alternativas)
rm(aux,mydata,i)

for (i in 1:length(alternativas)){
  k <- as.character(i)
plot(sum(alternativas[[i]]$tempo),sum(alternativas[[i]]$distancia),xlim=c(25,33),
     ylim=c(1600,1800),pch=1,xlab='Tempo total',ylab='Distância total', main="Alternativas para a Tomada de Decisão")
text(sum(alternativas[[i]]$tempo),sum(alternativas[[i]]$distancia),labels=as.character(i),cex=0.5,pos=3.2)  
par(new=T)
}
par(new=F)


#GRAFO

library(igraph)
ge1=graph.adjacency(adjmatrix=Sobreclassificacao,mode="directed",diag=FALSE)


plot(ge1,edge.arrow.size=.4,vertex.size=20,vertex.color="white",vertex.label.cex=0.9)



