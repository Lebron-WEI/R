f <- function(x){
  ans <- (1/sqrt(2*pi))*exp(-x^2/2)
  return(ans)
}

g <- function(x, y){
  ans <- (-1/2)*exp(-abs(x-y))
  return(ans)
}

x <- vector(length = 1000)
x[1] <- 0

for(i in 1:1000){
  s <- runif(n = 1, min = 0, max = 1)
  if( s <= 1/2 ){
    y <- x[i] + log(2*s)
    print(y)
  }
  if( s > 1/2 ){
    y <- x[i] - log(2*(1-s))
    print(y)
  }
  
  r <- (f(y)*g(x[i], y))/(f(x[i])*g(y, x[i]))
  
  if(r >= 1){
    x[i+1] <- y
  }
  
  else{
    u <- runif(n = 1, min = 0, max = 1)
    if(u <= r){
      x[i+1] <- y
    }
    else{
      x[i+1] <- x[i]
    }
  }
}

hist(x, main="Question_3", xlab = "X",  xlim = c(-5,5),breaks = 100)
d <- var(x)

x_sorted <- sort(x)

x_950 <- x_sorted[950]
x_975 <- x_sorted[975]

paste("95.0%分位点是",x_950)
paste("97.5%分位点是",x_975)
paste("样本方差是",d)