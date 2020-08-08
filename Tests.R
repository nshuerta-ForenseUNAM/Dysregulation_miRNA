#Tests

#Install packages (only once)
install.packages("readxl") #Excel reading
install.packages("raster") #Graphic design
install.packages("ggplot2") #Graphic design
install.packages("dplyr") #Graphic design
install.packages("hrbrthemes") #Graphic design
install.packages("ggpubr") #Graphic design
install.packages("gridExtra") #Graphic design

#Call library 
library(readxl) #Excel reading
library(raster) #Graphic design
library(ggplot2) #Graphic design
library(dplyr) #Graphic design
library(hrbrthemes) #Graphic design
library(ggpubr) #Graphic design
require(gridExtra) #Graphic design

#Data choice
dir<-file.choose()

#Data load 
#Data MicroRNA
xlsx1<-read_excel(dir,sheet=1,col_names=TRUE)
data<-as.data.frame(xlsx1)
head(data)

data$Time<-as.factor(data$Time)

#Simple graphics
P1 <- ggline(data, x = "Time", y = "mir381", 
  palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                add = c("mean_se","jitter"))+ labs(y = "Fold change",x = "Time (hours)",add.params = list(color = "red"))+theme(text=element_text(size=14,family="Arial"))

P2 <- ggline(data, x = "Time", y = "mir23b", 
  palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                add = c("mean_se","jitter"))+ labs(y = "Fold change",x = "Time (hours)")+theme(text=element_text(size=14,family="Arial"))

P3 <- ggline(data, x = "Time", y = "mir144", 
  palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                add = c("mean_se","jitter"))+ labs(y = "Fold change",x = "Time (hours)")+theme(text=element_text(size=14,family="Arial"))

#mir381
my_comparisons <- list( c("0", "3"),c("0", "24") )
my_comparisons1 <- list( c("3", "12"))
my_comparisons2 <- list( c("3", "24") )

PP1<-P1+stat_compare_means(comparisons = my_comparisons,size = 4,symnum.args=list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("0.0001", "0.001", "p-value < 0.01", "p-value < 0.05","1"))) # Add pairwise comparisons p-value #+ stat_compare_means(label.y = 3) 
PP1<-PP1+stat_compare_means(comparisons = my_comparisons1,size = 4,label.y = 0,symnum.args=list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("0.0001", "0.001", "**", "*","1"))) # Add pairwise comparisons p-value
PP1<-PP1+stat_compare_means(comparisons = my_comparisons2,size = 4,label.y = -0.2,symnum.args=list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("0.0001", "0.001", "**", "*","1"))) # Add pairwise comparisons p-value
PPP1<-PP1+ stat_summary(geom = "point",shape = 15,size = 3,col = "black",fun.y = "mean")

#mir23b
my_com <- list( c("0", "24"))
my_com1 <- list( c("3", "24"))
my_com2 <- list( c("6", "24"))
my_com3 <- list( c("12", "24"))

PP2<-P2+stat_compare_means(comparisons = my_com,size = 4,symnum.args=list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("0.0001", "0.001","p-value < 0.01", "p-value < 0.05","1"))) # Add pairwise comparisons p-value
PP2<-PP2+stat_compare_means(comparisons = my_com3,size = 4,label.y = -0.2,symnum.args=list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("0.0001", "0.001", "**", "*","1"))) # Add pairwise comparisons p-value
PP2<-PP2+stat_compare_means(comparisons = my_com2,size = 4,label.y = -0.45,symnum.args=list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("0.0001", "0.001", "**", "*","1"))) # Add pairwise comparisons p-value
PP2<-PP2+stat_compare_means(comparisons = my_com1,size = 4,label.y = -0.7,symnum.args=list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("0.0001", "0.001", "**", "*","1"))) # Add pairwise comparisons p-value
PPP2<-PP2+ stat_summary(geom = "point",shape = 15,size = 3,col = "black",fun.y = "mean")

#mir144
PPP3<-P3+ stat_summary( geom = "point",shape = 15,size = 3,col = "black",fun.y = "mean")

#Directory local
getwd()

#Directory change 
setwd("Directory")

#Graphics - Comparative
plot<-grid.arrange(PPP1, PPP2,PPP3, ncol=3)

#Save graphics
g <- arrangeGrob(PPP1,PPP2,PPP3, ncol=3) 
ggsave(file="comparison.tiff", g,dpi = 320,width = 50, height = 20, units = "cm")

g <- arrangeGrob(PPP1, PPP2,PPP3, ncol=3) #generates g
 ggsave(file="comparison1.tiff", g,dpi = 320,width =350, height = 138, units = "mm")




