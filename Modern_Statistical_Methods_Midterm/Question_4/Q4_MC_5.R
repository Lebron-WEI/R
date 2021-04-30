n <- 100000
X <- vector(length = n)
W <- vector(length = n)
H <- vector(length = n)

e <- exp(1)

h <- function(x){
  ans <- ((e-1)/e)*(1/sqrt(2*pi))*exp(-x^2/2+x)
  return(ans)
}

p <- function(x){
  ans <- (e/(e-1))*e^(-x)*ifelse(x>0 && x<1, 1, 0)
  return(ans)
}

g <- function(x){
  ans <- (1/sqrt(2*pi))*exp(-x^2/2)
  return(ans)
}

for(i in 1:n){
  X[i] <- rnorm(1,mean = 0,sd = 1)

  W[i] <- p(X[i])/g(X[i])
  H[i] <- h(X[i])
}

ans <- sum(W*H)/n
ans_sd <- (sum((H-ans)^2))/(n*(n-1))

ans <- ans + (1/2)

paste("估计值为", ans)
paste("估计值的方差为", ans_sd)
paste("当前样本量大小是", n)

