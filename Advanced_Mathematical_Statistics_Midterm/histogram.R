n <- 100
Y <- vector(length = 1000)
a <- c(1:1000)
for(i in c(1:1000)){
  X <- rnorm(n, mean = 0, sd = 1)
  
  Y[i] <- -n*log(sum(X*X)/n-(sum(X)/n)^2)+sum(X*X)-n
}

# Create the histogram.
hist(Y, main="n=100ʱģ��ķֲ�", xlab = "-2log(LRTͳ����)",  xlim = c(-15,15),breaks = 100)
