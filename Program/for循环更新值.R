N <- 15
S <- 0
y <- vector(mode = "numeric",length=N)
for(i in c(1:N)){
  S <- S+factorial(i-1)*factorial(i-1)/(factorial(2*i-1)*i*(2*i-1))
  y[i]=S
}
print(y)
plot(y)