# Model 
#Install packages 
install.packages("readxl") #Excel reading
install.packages("raster") #Graphic design
install.packages("ggplot2") #Graphic design


#Call library 
library(readxl) #Excel reading
library(raster) #Graphic design
library(ggplot2) #Graphic design

#Data choice
dir<-file.choose()

#Data load 
#Data MicroRNA
xlsx1<-read_excel(dir,sheet=1,col_names=TRUE)
data<-as.data.frame(xlsx1)
head(data)

ols <-lm(data[,3]~ factor(Time)-1+factor(BL)+factor(CE), data = data)
summary(ols)

summary(ols)
confint(ols)
anova(ols)
r<-residuals(ols)
fitted(ols)
influence(ols)

yhat <- ols$fitted

yhat0<-yhat[1:5]
m0<-mean(yhat0)
s0<-sd(yhat0)
li0<-m0-(1.96*(s0/sqrt(5)))
ls0<-m0+(1.96*(s0/sqrt(5)))

yhat3<-yhat[6:10]
m3<-mean(yhat3)
s3<-sd(yhat3)
li3<-m3-(1.96*(s3/sqrt(5)))
ls3<-m3+(1.96*(s3/sqrt(5)))

yhat6<-yhat[11:15]
m6<-mean(yhat6)
s6<-sd(yhat6)
li6<-m6-(1.96*(s6/sqrt(5)))
ls6<-m6+(1.96*(s6/sqrt(5)))

yhat12<-yhat[16:20]
m12<-mean(yhat12)
s12<-sd(yhat12)
li12<-m12-(1.96*(s12/sqrt(5)))
ls12<-m12+(1.96*(s12/sqrt(5)))

yhat24<-yhat[21:25]
m24<-mean(yhat24)
s24<-sd(yhat24)
li24<-m24-(1.96*(s24/sqrt(5)))
ls24<-m24+(1.96*(s24/sqrt(5)))

li<-c(li0,li3,li6,li12,li24)
ls<-c(ls0,ls3,ls6,ls12,ls24)

#Graphics - Goodness of Fit
ggplot(data, aes(x = data[,2], y = data[,3]))+
  geom_point() +xlab("PMI")+ylab("Fold change")+
  geom_smooth(stat = "smooth",method=lm)
