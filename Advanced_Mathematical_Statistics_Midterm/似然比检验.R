n <- 50
a <- 0.1   #显著性水平
m <- 2  #泊松分布的参数
Y <- vector(length = 1000)   #生成一个向量用来计算统计量
Z <- rep(0, times = 1000)   #生成一个初始化向量用来记录次数
for(i in c(1:1000)){
  X <- rpois(n,m)#生成随机的服从于参数为m泊松分布的序列n个
  Y[i] <- 2*n-2*sum(X)+2*sum(X)*log(sum(X)/n)
  if(Y[i]>qchisq(1-a,1)){
    Z[i] <- 1
  }
}
p <- sum(Z)/1000
print(p)