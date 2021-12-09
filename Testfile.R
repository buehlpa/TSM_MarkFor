#Libraries

library(GGally)
library(readr)
library(ggplot2)
library(ggpubr)
library(scatterplot3d) 


#read the data and plot it
fish_dat=readr::read_csv("Fish.csv")                        #  reading as tibble
GGally::ggpairs(fish_dat,aes(color = Species)) + theme_bw() #  pairwise plot 
summary(fish_dat);str(fish_dat)  



# showing the whole transformation of weight
# fish_dat$Weight=sqrt(fish_dat$Weight)
# GGally::ggpairs(fish_dat,aes(color = Species)) + theme_bw() #  pairwise plot 

# Linear Regression####

# Use only Perch data


Perch_dat=fish_dat%>%dplyr::filter(Species=="Perch")



#split test train

#aprox 11 (80%)
#test  datapoints
#train  45 data points

dim(Perch_dat)
set.seed(42)
samp= sample(x = 1:56,size = 11,replace = FALSE)
Perch_train=Perch_dat[-samp,]
Perch_test=Perch_dat[samp,]



# comparison of targetvariable weigth with transformations
par(mfrow=c(1,3))
plot(Weight~Width,data=Perch_train)
plot(log(Weight)~Width,data=Perch_train)
plot(sqrt(Weight)~Width,data=Perch_train)


# a nicer version in ggplot
nPlt=ggplot(Perch_train,aes(Width,Weight))+geom_point()
logPlt=ggplot(Perch_train,aes(Width,log(Weight)))+geom_point()
sqrtPlt=ggplot(Perch_train,aes(Width,sqrt(Weight)))+geom_point()
figure <- ggarrange(nPlt, logPlt, sqrtPlt,labels = c("A", "B", "C"),ncol = 3, nrow = 1)


#SLR 
Perch_fit=lm(sqrt(Weight)~Width,data = Perch_train)

#residual analysis  
par(mfrow=c(1,4))
plot(Perch_fit)

AIC(Perch_fit)
car::vif(Perch_fit)
drop1(Perch_fit)






#Full Model
Perch_fit_full=lm(sqrt(Weight)~.,data = Perch_train[,-1]) 



#MAD
mad(predict(Perch_fit_full,Perch_test)-sqrt(Perch_test$Weight))            # 0.4947488
mad(predict(Perch_fit,Perch_test)-sqrt(Perch_test$Weight))                 # 0.4694862

#MSE
0.1*(sum((predict(Perch_fit_full,Perch_test)-sqrt(Perch_test$Weight))^2))  # 1.045417   #exactly here is the problem with mse
0.1*(sum((predict(Perch_fit,Perch_test)-sqrt(Perch_test$Weight))^2))       # 1.539654




library(lattice)
?cloud



Perch_fit_two=lm(Weight~.,data = Perch_sub) 


Perch_sub=Perch_train[,c(2,6,7)]
Perch_sub$Weight=sqrt(Perch_sub$Weight)

# 3D scatter plot
s3d <- scatterplot3d(Perch_sub, type = "p", color = "blue",
                     angle=55, pch = 16)
# Add regression plane
s3d$plane3d(Perch_fit_two)


install.packages("rockchalk")


library(rockchalk)

#m1 = lm(mpg ~ poly(wt,2) + disp, data=mtcars)

old.par = par(mfrow=c(1,2), mar=c(1,1,1,1))

plotPlane(Perch_fit_two, "Height", "Width", pch=16, pcol=rgb(91,179,0,maxColorValue=255),col=rgb(0,0,1,0.1), drawArrows=TRUE, alength=0, 
          acol="red", alty=1,alwd=3, theta=5, phi=20)



plotPlane(m1, "wt", "disp", pch=16, col=rgb(0,0,1,0.1), drawArrows=TRUE, alength=0, 
          acol="red", alty=1,alwd=1, theta=35, phi=25)



#deccorelate d ata


?rgb

