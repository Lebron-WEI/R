#EM算法
t <- 1   #参数的初始值
precision <- 0.001   #精确度
max_loop <- 1000   #最大循环次数
diff <- 1
loop <- 0
f <- expression((6.25*t^2+5*t+2)/(t*(2.5*t+1)))
while (diff >= precision && loop <= max_loop) {
  loop <- loop + 1
  ft <- eval(f)
  
  s <- 2*15/(15.381+5*ft)
  diff <- s-t
  t <- s
  diff <- abs(diff)
}
print(t)