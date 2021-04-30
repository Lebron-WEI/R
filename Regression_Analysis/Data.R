library(openxlsx)

#处理缺失数据
data <- read.xlsx("Regression_Analysis/2020年回归分析期中考试数据.xlsx")
data_na <- is.na(data)

#每行缺失值的个数
num_na <- rowSums(data_na)

#将缺失值个数少于21的数据留下
newdata <- data[num_na<21,]

#总体的均值
data_info <- read.xlsx("Regression_Analysis/2020年回归分析期中考试数据说明.xlsx")
mean_data <- data_info$均值

#求一个向量的众数函数
zhongshu <- function(x)
{
  return(as.numeric(names(table(x))[table(x) == max(table(x))]))
}

#用均值代替缺失值
for(i in c(1,5:15,34,35,36,51:54,56:60,63:72)){
  newdata[is.na(newdata[,i]),i] <- mean_data[i]
}

#用众数代替缺失值
for(i in c(2:4,16:33,37:50,55,61,62)){
  newdata[is.na(newdata[,i]),i] <- zhongshu(data[,i])
}

#基于newdata引入哑变量
data0 <- newdata
fac <- c("GENDER","DRWITHGL","NRWITHGL","ETEST","CHGLASS1","TRT","AMB11","AMB21","AMB31","AMB41","DRUGIF","PREGQ","SGAR","LSMK","COSTM","BULBP","TUTOR1","TUTOR2","NTON","MSMK","HSMK","BED","IFTAI","BULB","TUTOR","CELLP","NOONS","EYE","EYE1","SITE","STR","PLACE5","SHIFT","IFDOU","ALLERGY","DOMEYE","DAI1","DAI2")
data0[,which(names(data0) %in% fac)] <-
  as.data.frame(lapply(data0[,which(names(data0) %in% fac)],as.factor))

newdata <- data0

#逐步回归进行变量选择
lm_newdata <- lm(RA~., data=newdata)
lm_newdata_step <- step(lm_newdata,direction = "both")
summary(lm_newdata_step)

#去掉不显著的变量FW,DRWBASE
lm_newdata_final=lm(RA ~ GENDER + NRWITHGL + RAXISLEG + RANTECHA + RNCONPRE + 
                      DBLOPRE + CHAOD1 + AMB41 + DRUGIF + BULBP + FH + CELLP + 
                      EYE1 + STR + DRNBASE + NRNBASE + DOMEYE + RPR + 
                      YTR + RCORCU + JNGLASS + DAI1 + PREG, data = newdata)
summary(lm_newdata_final)

#异常值和强影响点
test_outlier <- data.frame(rstudent(lm_newdata_final),
                           hatvalues(lm_newdata_final),
                           cooks.distance(lm_newdata_final))

#删除学生化内残差判断异常值
newdata_1 <- newdata[test_outlier[,1]<=3,]
#杠杆值和库克距离判断异常的强影响点
newdata_2 <- newdata_1[test_outlier[,2]<0.018152 || test_outlier[,3]<=1,]

#基于数据集newdata_2重新引入哑变量
data0 <- newdata_2
fac <- c("GENDER","DRWITHGL","NRWITHGL","ETEST","CHGLASS1","TRT","AMB11","AMB21","AMB31","AMB41","DRUGIF","PREGQ","SGAR","LSMK","COSTM","BULBP","TUTOR1","TUTOR2","NTON","MSMK","HSMK","BED","IFTAI","BULB","TUTOR","CELLP","NOONS","EYE","EYE1","SITE","STR","PLACE5","SHIFT","IFDOU","ALLERGY","DOMEYE","DAI1","DAI2")
data0[,which(names(data0) %in% fac)] <-
  as.data.frame(lapply(data0[,which(names(data0) %in% fac)],as.factor))

newdata_2 <- data0

#再做回归
lm_newdata_2_final_1=lm(RA ~ GENDER + NRWITHGL + RAXISLEG + RANTECHA + RNCONPRE + 
                      DBLOPRE + CHAOD1 + AMB41 + DRUGIF + BULBP + FH + CELLP + 
                      EYE1 + STR + DRNBASE + NRNBASE + DOMEYE + RPR + 
                      YTR + RCORCU + JNGLASS + DAI1 + PREG, data = newdata_2)
summary(lm_newdata_2_final_1)

#删除不显著的变量FH和JNGLASS
lm_newdata_2_final_2=lm(RA ~ GENDER + NRWITHGL + RAXISLEG + RANTECHA + RNCONPRE + 
                          DBLOPRE + CHAOD1 + AMB41 + DRUGIF + BULBP + CELLP + 
                          EYE1 + STR + DRNBASE + NRNBASE + DOMEYE + RPR + 
                          YTR + RCORCU + DAI1 + PREG, data = newdata_2)
summary(lm_newdata_2_final_2)

#删除不显著的变量DBLOPRE
lm_newdata_2_final_3=lm(RA ~ GENDER + NRWITHGL + RAXISLEG + RANTECHA + RNCONPRE + 
                          CHAOD1 + AMB41 + DRUGIF + BULBP + CELLP + 
                          EYE1 + STR + DRNBASE + NRNBASE + DOMEYE + RPR + 
                          YTR + RCORCU + DAI1 + PREG, data = newdata_2)
summary(lm_newdata_2_final_3)

#测试集

#测试集数据
data.test <- read.xlsx("Regression_Analysis/2020年回归分析期中考试测试集.xlsx")

#基于数据集newdata_2重新引入哑变量
data0 <- data.test
fac <- c("GENDER","DRWITHGL","NRWITHGL","ETEST","CHGLASS1","TRT","AMB11","AMB21","AMB31","AMB41","DRUGIF","PREGQ","SGAR","LSMK","COSTM","BULBP","TUTOR1","TUTOR2","NTON","MSMK","HSMK","BED","IFTAI","BULB","TUTOR","CELLP","NOONS","EYE","EYE1","SITE","STR","PLACE5","SHIFT","IFDOU","ALLERGY","DOMEYE","DAI1","DAI2")
data0[,which(names(data0) %in% fac)] <-
  as.data.frame(lapply(data0[,which(names(data0) %in% fac)],as.factor))

data.test <- data0

predict(lm_newdata_2_final_3,data.test)