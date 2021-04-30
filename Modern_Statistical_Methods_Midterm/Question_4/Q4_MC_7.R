n <- 100
X <- vector(length = n)
H <- vector(length = n)
J <- vector(length = n)
Q <- vector(length = n)

p <- function(x){
  if(0 < x && x < 1){
    ans <- 1
  }
  else{
    ans <- 0
  }
  return(ans)
}

h <- function(x){
  ans <- (1/sqrt(2*pi))*exp(-x^2/2)
  return(ans)
}

q <- function(x){
  ans <- -x^2+x
  return(ans)
}

for(i in 1:n){
  #根据密度函数p生成X，但是已知了密度函数p是均匀分布所以直接用runif函数
  X[i] <- runif(1, min = 0, max = 1)
  
  H[i] <- h(X[i])
  J[i] <- h(1-X[i])
  Q[i] <- q(X[i])
}

mu <- 1/6
lam <- sum((H/2+J/2-(sum(H/2)+sum(J/2))/n)*(Q-mu))/(sum((Q-mu)^2))

ans <- sum(H/2+J/2-lam*(Q-mu))/n
ans_sd <- ((1/2)*var(H) + (1/2)*cov(H, J))/n
ans <- ans + 1/2

paste("估计值为", ans)
paste("估计值的方差为", ans_sd)
paste("当前样本量大小是", n)