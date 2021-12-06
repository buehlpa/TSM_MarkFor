fish_dat=readr::read_csv("Fish.csv")                        #  reading as tibble
GGally::ggpairs(fish_dat,aes(color = Species)) + theme_bw() #  pairwise plot 
summary(fish_dat);str(fish_dat)  






Perch_dat=fish_dat%>%dplyr::filter(Species=="Perch")
   #  Use only Perch data
par(mfrow=c(1,3))
plot(Weight~Width,data=Perch_dat)
plot(log(Weight)~Width,data=Perch_dat)
plot(sqrt(Weight)~Width,data=Perch_dat)

library("ggpubr")
library(ggplot2)

nPlt=ggplot(Perch_dat,aes(Width,Weight))+geom_point()
logPlt=ggplot(Perch_dat,aes(Width,log(Weight)))+geom_point()
sqrtPlt=ggplot(Perch_dat,aes(Width,sqrt(Weight)))+geom_point()
figure <- ggarrange(nPlt, logPlt, sqrtPlt,labels = c("A", "B", "C"),ncol = 3, nrow = 1)




