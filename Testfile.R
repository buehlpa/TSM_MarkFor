fish_dat=readr::read_csv("Fish.csv")                        #  reading as tibble
GGally::ggpairs(fish_dat,aes(color = Species)) + theme_bw() #  pairwise plot 
summary(fish_dat);str(fish_dat)  






Perch_dat=fish_dat%>%dplyr::filter(Species=="Perch")


dim(Perch_dat)

# Use only Perch data

#aprox 11 (80%)
#test  datapoints
#train  45 data points
set.seed(42);samp= sample(x = 1:56,size = 11,replace = FALSE)
Perch_dat=Perch_dat[-samp,]

Perch_fit=lm(log(Weight)~Width,data = Perch_train)

  


par(mfrow=c(1,3))
plot(Weight~Width,data=Perch_dat)
plot(1/Weight)~Width,data=Perch_dat)
plot(sqrt(Weight)~Width,data=Perch_dat)

library("ggpubr")
library(ggplot2)

nPlt=ggplot(Perch_dat,aes(Width,Weight))+geom_point()
logPlt=ggplot(Perch_dat,aes(Width,log(Weight)))+geom_point()
sqrtPlt=ggplot(Perch_dat,aes(Width,sqrt(Weight)))+geom_point()
figure <- ggarrange(nPlt, logPlt, sqrtPlt,labels = c("A", "B", "C"),ncol = 3, nrow = 1)



par(mfrow=c(2,2))
plot(Perch_fit)

Perch_fit=lm(sqrt(Weight)~.,data = Perch_dat[,-1]) #  linear Regression model

library(car)

summary(Perch_fit)
car::vif(Perch_fit)
drop1(Perch_fit)
?drop1




