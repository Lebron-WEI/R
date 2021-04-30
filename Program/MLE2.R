p <- function(w,t){
  ans <- w[1]*exp(-w[2]*t)
  return(ans)
}

lnl<- function(w,Y,Time) # 创建似然函数
{	
  n <- 100
  temp1 <- n*Y*log(p(w,Time))
  temp2 <- n*(1-Y)*log(1-p(w,Time))
  ans<- sum(temp1)+sum(temp2)
  return(-ans)
}

w0 = c(1.1, 0.4) #初始化两个参数作为迭代初始值

#表格中的数据
Y0 = c(0.94,0.77,0.40,0.26,0.24,0.16)
T0 = c(1,3,6,9,12,18)

result=optim(par = w0,fn = lnl,Y=Y0,Time=T0)
result