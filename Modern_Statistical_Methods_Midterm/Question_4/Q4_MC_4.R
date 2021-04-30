n <- 10000
X <- vector(length = n)
H <- vector(length = n)

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

for(i in 1:n){
  #根据密度函数p生成X，但是已知了密度函数p是均匀分布所以直接用runif函数
  X[i] <- runif(1, min = 0, max = 1)
  
  H[i] <- h(X[i])
}

ans <- sum(H)/n
ans_sd <- (sum((H-ans)^2))/(n*(n-1))
ans <- ans + 1/2

paste("估计值为", ans)
paste("估计值的方差为", ans_sd)
paste("当前样本量大小是", n)