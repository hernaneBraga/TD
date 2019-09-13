newsolution <- function(x, n){
y <- x
i <- n
from <- which(y[,i]==1)
j <- which(y[i,]==1)
to <-  which(y[j,]==1)
y[from,i] <- 0
y[from,j] <- 1
y[i,j] <- 0
y[j,i] <- 1
y[j,to] <- 0
y[i,to] <- 1
return(y)
}