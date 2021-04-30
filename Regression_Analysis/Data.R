library(openxlsx)

#����ȱʧ����
data <- read.xlsx("Regression_Analysis/2020��ع�������п�������.xlsx")
data_na <- is.na(data)

#ÿ��ȱʧֵ�ĸ���
num_na <- rowSums(data_na)

#��ȱʧֵ��������21����������
newdata <- data[num_na<21,]

#����ľ�ֵ
data_info <- read.xlsx("Regression_Analysis/2020��ع�������п�������˵��.xlsx")
mean_data <- data_info$��ֵ

#��һ����������������
zhongshu <- function(x)
{
  return(as.numeric(names(table(x))[table(x) == max(table(x))]))
}

#�þ�ֵ����ȱʧֵ
for(i in c(1,5:15,34,35,36,51:54,56:60,63:72)){
  newdata[is.na(newdata[,i]),i] <- mean_data[i]
}

#����������ȱʧֵ
for(i in c(2:4,16:33,37:50,55,61,62)){
  newdata[is.na(newdata[,i]),i] <- zhongshu(data[,i])
}

#����newdata�����Ʊ���
data0 <- newdata
fac <- c("GENDER","DRWITHGL","NRWITHGL","ETEST","CHGLASS1","TRT","AMB11","AMB21","AMB31","AMB41","DRUGIF","PREGQ","SGAR","LSMK","COSTM","BULBP","TUTOR1","TUTOR2","NTON","MSMK","HSMK","BED","IFTAI","BULB","TUTOR","CELLP","NOONS","EYE","EYE1","SITE","STR","PLACE5","SHIFT","IFDOU","ALLERGY","DOMEYE","DAI1","DAI2")
data0[,which(names(data0) %in% fac)] <-
  as.data.frame(lapply(data0[,which(names(data0) %in% fac)],as.factor))

newdata <- data0

#�𲽻ع���б���ѡ��
lm_newdata <- lm(RA~., data=newdata)
lm_newdata_step <- step(lm_newdata,direction = "both")
summary(lm_newdata_step)

#ȥ���������ı���FW,DRWBASE
lm_newdata_final=lm(RA ~ GENDER + NRWITHGL + RAXISLEG + RANTECHA + RNCONPRE + 
                      DBLOPRE + CHAOD1 + AMB41 + DRUGIF + BULBP + FH + CELLP + 
                      EYE1 + STR + DRNBASE + NRNBASE + DOMEYE + RPR + 
                      YTR + RCORCU + JNGLASS + DAI1 + PREG, data = newdata)
summary(lm_newdata_final)

#�쳣ֵ��ǿӰ���
test_outlier <- data.frame(rstudent(lm_newdata_final),
                           hatvalues(lm_newdata_final),
                           cooks.distance(lm_newdata_final))

#ɾ��ѧ�����ڲв��ж��쳣ֵ
newdata_1 <- newdata[test_outlier[,1]<=3,]
#�ܸ�ֵ�Ϳ�˾����ж��쳣��ǿӰ���
newdata_2 <- newdata_1[test_outlier[,2]<0.018152 || test_outlier[,3]<=1,]

#�������ݼ�newdata_2���������Ʊ���
data0 <- newdata_2
fac <- c("GENDER","DRWITHGL","NRWITHGL","ETEST","CHGLASS1","TRT","AMB11","AMB21","AMB31","AMB41","DRUGIF","PREGQ","SGAR","LSMK","COSTM","BULBP","TUTOR1","TUTOR2","NTON","MSMK","HSMK","BED","IFTAI","BULB","TUTOR","CELLP","NOONS","EYE","EYE1","SITE","STR","PLACE5","SHIFT","IFDOU","ALLERGY","DOMEYE","DAI1","DAI2")
data0[,which(names(data0) %in% fac)] <-
  as.data.frame(lapply(data0[,which(names(data0) %in% fac)],as.factor))

newdata_2 <- data0

#�����ع�
lm_newdata_2_final_1=lm(RA ~ GENDER + NRWITHGL + RAXISLEG + RANTECHA + RNCONPRE + 
                      DBLOPRE + CHAOD1 + AMB41 + DRUGIF + BULBP + FH + CELLP + 
                      EYE1 + STR + DRNBASE + NRNBASE + DOMEYE + RPR + 
                      YTR + RCORCU + JNGLASS + DAI1 + PREG, data = newdata_2)
summary(lm_newdata_2_final_1)

#ɾ���������ı���FH��JNGLASS
lm_newdata_2_final_2=lm(RA ~ GENDER + NRWITHGL + RAXISLEG + RANTECHA + RNCONPRE + 
                          DBLOPRE + CHAOD1 + AMB41 + DRUGIF + BULBP + CELLP + 
                          EYE1 + STR + DRNBASE + NRNBASE + DOMEYE + RPR + 
                          YTR + RCORCU + DAI1 + PREG, data = newdata_2)
summary(lm_newdata_2_final_2)

#ɾ���������ı���DBLOPRE
lm_newdata_2_final_3=lm(RA ~ GENDER + NRWITHGL + RAXISLEG + RANTECHA + RNCONPRE + 
                          CHAOD1 + AMB41 + DRUGIF + BULBP + CELLP + 
                          EYE1 + STR + DRNBASE + NRNBASE + DOMEYE + RPR + 
                          YTR + RCORCU + DAI1 + PREG, data = newdata_2)
summary(lm_newdata_2_final_3)

#���Լ�

#���Լ�����
data.test <- read.xlsx("Regression_Analysis/2020��ع�������п��Բ��Լ�.xlsx")

#�������ݼ�newdata_2���������Ʊ���
data0 <- data.test
fac <- c("GENDER","DRWITHGL","NRWITHGL","ETEST","CHGLASS1","TRT","AMB11","AMB21","AMB31","AMB41","DRUGIF","PREGQ","SGAR","LSMK","COSTM","BULBP","TUTOR1","TUTOR2","NTON","MSMK","HSMK","BED","IFTAI","BULB","TUTOR","CELLP","NOONS","EYE","EYE1","SITE","STR","PLACE5","SHIFT","IFDOU","ALLERGY","DOMEYE","DAI1","DAI2")
data0[,which(names(data0) %in% fac)] <-
  as.data.frame(lapply(data0[,which(names(data0) %in% fac)],as.factor))

data.test <- data0

predict(lm_newdata_2_final_3,data.test)