n <- 100000
X <- vector(length = n)
H <- vector(length = n)

p <- function(x){
  ans <- (1/sqrt(2*pi))*exp(-x^2/2)
  return(ans)
}

h <- function(x){#h取示性函数
  ans <- ifelse(abs(x) > 1, 1, 0)
  return(ans)
}

for(i in 1:n){
  #根据密度函数p生成X，但是已知了密度函数p是正态分布所以直接用rnorm函数
  X[i] <- rnorm(1,mean = 0,sd = 1)
  
  H[i] <- h(X[i])
}

ans <- sum(H)/n
ans_sd <- (sum((H-ans)^2))/(4*n*(n-1))
ans <- 1-(sum(H)/n)/2

paste("估计值为", ans)
paste("估计值的方差为", ans_sd)
paste("当前样本量大小是", n)