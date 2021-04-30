library(openxlsx)
data <- read.xlsx("Advanced_Mathematical_Statistics_Midterm/Newton_Raphson/data1.xlsx")
R <- data$X/data$Y

Fisher_Scoring <- function(f, df, init_value, precision, max_loop) {
  loop <- 0
  diff <- 1
  x <- init_value
  while (diff >= precision && loop <= max_loop) {
    loop <- loop + 1
    fx <- eval(f)
    dfx <- eval(df)
    if (dfx == 0) {
      warning("break!")
      break
    }
    diff <- -fx/dfx
    x <- x + diff
    print(x)
    diff <- abs(diff)
  }
}

f <- expression(20/x-2*(sum(R/(1+x*R))))
df <- expression(-20/(3*(x^2)))
init_value <- 1
Fisher_Scoring(f,df,init_value,0.000000001,100)