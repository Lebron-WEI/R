X <- matrix(nrow = 1001, ncol = 2)

f <- function(x){#x是二维向量
  V <- matrix(c(1,1/4,1/4,1),nrow = 2,ncol = 2, byrow = TRUE)
  ans <- (1/sqrt((2*pi)^2*det(V)))*exp((-1/2)*t(x)%*%solve(V)%*%x)
  return(ans[1,1])
}

g <- function(y){
  ans <- (1/2)*exp(-sqrt(sum(y^2)))
  return(ans)
}

i <- 1

c <- 3

y <- vector(mode = "numeric", length = 2)

while(is.na(X[1001,1]) == TRUE){
  s <- runif(n = 1, min = 0, max = 1)
  if( s <= 1/2 ){
    y[1] <- log(2*s)
  }
  if( s > 1/2 ){
    y[1] <- -log(2*(1-s))
  }
  
  t <- runif(n = 1, min = 0, max = 1)
  if( t <= 1/2 ){
    y[2] <- log(2*t)
  }
  if( t > 1/2 ){
    y[2] <- -log(2*(1-t))
  }
  
  u <- runif(n = 1, min = 0, max = 1)
  
  if( u < f(y)/(c*g(y[1])*g(y[2])) ){
    X[i,] <- y
    i <- i + 1
  }
}

hist(X[-1001,1], main="X[,1]", xlab = "X",  xlim = c(-5,5),breaks = 100)
hist(X[-1001,2], main="X[,2]", xlab = "X",  xlim = c(-5,5),breaks = 100)
cov(X[-1001,1],X[-1001,2])/(sqrt(var(X[-1001,1])*var(X[-1001,2])))