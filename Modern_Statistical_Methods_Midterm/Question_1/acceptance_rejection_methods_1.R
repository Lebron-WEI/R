X <- vector(length = 501)

a <- 2
b <- 9
c <- 4

pow <- function(s,t){
  return(s^t)
}

f <- function(x, a, b){
  ans <- gamma(a+b)*pow(x,a-1)*pow(1-x, b-1)/( gamma(a)*gamma(b) )
  return(ans)
}

i <- 1

while(X[501] == FALSE){
  y <- runif(n = 1, min = 0, max = 1)
  u <- runif(n = 1, min = 0, max = 1)
  if( u < f(y, a, b)/(c*y) ){
    X[i] <- y
    i <- i + 1
  }
}


hist(X[-501], main="Beta(2, 9)", xlab = "X",  xlim = c(0,1),breaks = 200)
paste("acceptance rate =",c)