f<-function(k){
  -beta(k+12,k+8)/beta(k,k)
}
ML.op=optimize(f,interval = c(0,700))#不需要参数初值，interval定义参数取值范围
ML.op
