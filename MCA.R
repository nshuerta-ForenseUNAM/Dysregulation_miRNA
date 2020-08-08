#MCA
#Install packages 
install.packages("readxl") #Excel reading
install.packages("polycor") #Polychoric and polyserial correlations 
install.packages("FactoMineR") #MCA
install.packages("factoextra") #Graphic design MCA
install.packages("ggrepel") #Graphic design 
install.packages("magick") #Graphic design 
install.packages("cowplot") #Graphic design 
install.packages("tidyverse") #Graphic design 

#Call library 
library(readxl) #Excel reading
library(polycor) #Polychoric and polyserial correlations 
library(FactoMineR) #MCA
library(factoextra) #Graphic design MCA
library(ggrepel) #Graphic design
library(magick) #Graphic design
library(cowplot) #Graphic design
library(tidyverse) # Modern data science library 

#Data choice 
dir<-file.choose()

#Data load 
#Data MicroRNA-mir381 
xlsx1<-read_excel(dir,sheet=1,col_names=TRUE)
data<-as.data.frame(xlsx1)
head(data)

#Polyserial correlation 
data1<-data[6:25,]
data1$Time<-as.factor(data1$Time)
# Time & mir381
polyserial(data1[,3],data1[,2],ML=TRUE, std.err=TRUE)

#Select data
data_mca<-data[,c("Time","mir381","RMU","RML","ED","AGD","BL","CE","DL","LLC","MML","BS","AS","LMC")]
nam<-c("Time","mir381","RMU","RML","ED","AGD","BL","CE","DL","LLC","MML","BS","AS","LMC")

#MCA
res.mca <- MCA(data_mca,ncp = 2,quanti.sup = 2,quali.sup = 1,graph=FALSE) # Supplementary quantitative variable # Supplementary qualitative variable
              
pos_vector <- rep(3, length(nam))
pos_vector[nam %in% c("BS_0","BL_1","LLC_1","RMU_1")] <- 4
pos_vector[nam %in% c("LMC_1","MML_1","CE_1","DL_1","ED_0","LMC_0")] <- 2
pos_vector[nam %in% c("MML_0","BS_1")] <- 1

d<-as.data.frame(res.mca$var$coord)
colnames(d)<-c("D1","D2")
nam<-rownames(d)
d1<-as.data.frame(res.mca$ind$coord)
colnames(d1)<-c("D1","D2")
d1$Time<-data_cor1[,"Time"]
d1$Groups <-data_cor[,"CLASS1"]
d1$Groups<-as.factor(d1$Groups)

names(d1)[3] <- "PMI"

colors<-c("#800000","#800000","#B22222","#B22222","#FF0000","#FF0000","#FF6347","#FF6347","#FF7F50","#FF7F50","#CD5C5C","#CD5C5C","#F08080","#F08080","#FFA07A","#FFA07A","#FF8C00","#FF8C00","#FFD700","#FFD700","#32CD32","#32CD32","#008000","#008000","#006400","#006400")

p1<- ggplot(d, aes(D1, D2,col=colors,label = rownames(d)))+geom_point(shape=0,show.legend = FALSE)+geom_text_repel(col="black",segment.color="gray",segment.alpha=.1,size = 2.5,show.legend = FALSE)+xlim(-2.5, 2.5) + ylim (-1.5, 2.5)+ labs(title ="")+ theme_minimal_grid()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+labs(x = "Dimension 1 (52.1%)",y = "Dimension 2 (21.1%)")

p2<-ggplot(d1,aes(x=D1, y=D2,color=PMI))+geom_point(size =1.5)+xlim(-2.5, 2.5) + ylim (-1.5, 2.5)+ labs(title ="")+stat_ellipse(type = "norm", linetype = 2)+ theme_minimal_grid()+theme(legend.position = "none",panel.grid.major = element_blank()) +geom_vline(xintercept = 0,colour = "gray")+geom_hline(yintercept = 0,colour = "gray")+labs(x = "Dimension 1 (52.1%)",y = "Dimension 2 (21.1%)")

p5<-ggdraw(clip = "on") +  draw_plot(p1,x = 0, y = 0)+draw_plot(p2,x = 0, y = 0)


legend_b <- get_legend(
  p2 + 
    guides(color = guide_legend(ncol = 1)) +
    theme(legend.position = "left",legend.justification = c("left", "top"),
    legend.box.just = "right")
)
#Graphics - original

p6<-plot_grid(p5, legend_b,rel_widths = c(1.5, .2))

p11<- ggplot(d, aes(D1, D2,col=colors,label = rownames(d)))+geom_point(shape=0,show.legend = FALSE)+geom_text_repel(col="black",segment.color="gray",segment.alpha=.1,size = 2.5,show.legend = FALSE)+xlim(-2.5, 2.5) + ylim (-1.5, 2.5)+ labs(title ="")+ theme_minimal_grid()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+labs(x = "Dimension 1 (52.1%)",y = "Dimension 2 (21.1%)")

p22<-ggplot(d1,aes(x=D1, y=D2,color=Groups))+geom_point(size =1.5)+xlim(-2.5, 2.5) + ylim (-1.5, 2.5)+ labs(title ="")+stat_ellipse(type = "norm", linetype = 2)+ theme_minimal_grid()+theme(legend.position = "none",panel.grid.major = element_blank()) +geom_vline(xintercept = 0,colour = "gray")+geom_hline(yintercept = 0,colour = "gray")+labs(x = "Dimension 1 (52.1%)",y = "Dimension 2 (21.1%)")

p55<-ggdraw(clip = "on") +  draw_plot(p11,x = 0, y = 0)+draw_plot(p22,x = 0, y = 0)

legend_b <- get_legend(
  p22 + 
    guides(color = guide_legend(ncol = 1)) +
    theme(legend.position = "left",legend.justification = c("left", "top"),
    legend.box.just = "right")
)

#Graphics - per group
p66<-plot_grid(p55, legend_b,rel_widths = c(1.5, .2), ncol = 2)

#Save graphics
ggsave(file="p6_1.tiff", p6,dpi = 320,width =200, height = 200, units = "mm")

ggsave(file="p66_1.tiff", p66,dpi = 320,width =200, height = 200, units = "mm")

