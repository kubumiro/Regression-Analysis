
install.packages("car")
install.packages("MASS")
install.packages("ggplot2")
install.packages("ISLR")
install.packages("graphics")
install.packages("effects")
install.packages("lattice")
install.packages("leaps")
install.packages("psych")
install.packages("lmtest")
install.packages("robustbase")
install.packages("splines")
install.packages("markdown")
install.packages("rmarkdown")
install.packages("knitr")
install.packages("GGally")
install.packages("olsrr")

getwd()  # find where are you at the moment 
setwd("C:/Users/User/Desktop/REAN2018_zapocet")  
cars = read.csv("Car_data.csv",  header = TRUE, sep=";")


library(olsrr)
library(GGally)
library(splines)
library(lattice)
library(MASS)
library(car)
library(ggplot2)
library(ISLR)
library(graphics)
library(effects)
library(leaps)
library(psych)
library(robustbase)
library(lmtest)



summary(cars)

#Q1

sum(is.na(cars)) #ukaze, jestli jsou v datech NA (resp. pocet NA) --> nejsou
    #kvalitativni i kvanti = cylindres, doors
    #kvalit = cruise (tempomat), leather, sound, type, make, model, trim
    #3.5 dveri asi mit nebudu, navic se to bude lepe zobrazovat
cars$Doors <- factor(cars$Doors)
cars$Cylinder <- factor(cars$Cylinder)
cars$Leather <- factor(cars$Leather)
cars$Sound <- factor(cars$Sound)
cars$Cruise <- factor(cars$Cruise)
cars$Make <- factor(cars$Make)
cars$Type <- factor(cars$Type)

#Q2


cars$Odometer <- (cars$Mileage*1.6)
summary(cars$Odometer)
cars$Mileage<- NULL
cars$Price <- (cars$Price*22)
hist(cars$Odometer, prob = T)
lines(density(cars$Odometer,na.rm=TRUE),col="red")#remova NA = true
hist(cars$Price, prob = T)
lines(density(cars$Price,na.rm=TRUE),col="blue")

attach(cars)

#Q3

plot(Price ~ Odometer,cars,main="Nadpis",xlab="Odometer", ylab="Price")
abline(lm(cars$Price~cars$Odometer))
lines(lowess(cars$Odometer,cars$Price), col = "blue")
#porovnavame ruzne segmenty aut, tj, neda se moc porovnavat, potreboval bych porovnavat kategorie zvlast

#Q4


boxplot(Price~Make,data=cars,main="Different boxplots for each make", xlab="Make", ylab="Price", 
        col="red", border="brown")

boxplot(Price~Type,data=cars,main="Different boxplots for each make", xlab="Make", ylab="Price", 
        col="red", border="brown")

boxplot(Price~Doors,data=cars,main="Different boxplots for each make", xlab="Make", ylab="Price", 
        col="red", border="brown")

boxplot(Price~Cruise,data=cars,main="Different boxplots for each make", xlab="Make", ylab="Price", 
        col="red", border="brown")

boxplot(Price~Sound,data=cars,main="Different boxplots for each make", xlab="Make", ylab="Price", 
        col="red", border="brown")

boxplot(Price~Leather,data=cars,main="Different boxplots for each make", xlab="Make", ylab="Price", 
        col="red", border="brown")

#nic neslucujeme

#Q5


ggplot(cars, aes(Price,Type)) + 
  geom_point(aes(color = Make))

ggplot(cars, aes(Price,Make)) + 
  geom_point(aes(color = Type))

#Q6


ggplot(cars[Make=="SAAB",], aes(Odometer,Price)) + 
  geom_point(aes(size = Liter,color = Type)) # + scale_size_area()

#Q7 - navrhnete dalsi zobrazeni - DU

#Q8

model = lm(Price ~ Odometer, cars[Make=="Saturn",])
summary(model) #t-test i F-test sedi
coef(model)
plot(Price ~ Odometer,cars[Make=="Saturn",],
     main="nadpis",xlab="Odometer", ylab="Price ")
abline(model,col ="blue")

layout(matrix(1:4,2,2))
plot(model)

hist(fitted.values(model))
hist(residuals(model))

shapiro.test(residuals(model)) #normalita rezidui
# Breusch-Pagan test statistic
bptest(model) #mame problem s heteroskedasticitou
qqnorm(residuals(model))
qqline(residuals(model), col = 2)
#seriova nezavislost je ok, protoze mam jen jednu promennou

#Q9

model1 <- lm(log(Price) ~ Odometer , cars[Make=="Saturn",])
summary(model1)
coef(model1)

bptest(model1)
shapiro.test(residuals(model1)) 

plot(log(Price) ~ Odometer,cars[Make=="Saturn",],
     main="nadpis",xlab="Odometer", ylab="Price ")
abline(model1,col ="red")

#zlepsili jsme heteroskedasticitu, ta je nyni OK, ale tesne neprosla normalita rezidui

#Q10
model2 <- lm(Price ~ bs(Odometer,knots=c(20000,40000)), cars[Make=="Saturn",])
points(Odometer.grid,predict(model2,newdata = list(Odometer=Odometer.grid)),col="darkgreen",lwd=2,type="l")
#adding cutpoints
summary(model2)

plot(Odometer~Price, cars[Make=="Saturn",],col="grey",xlab="Age",ylab="Wages")

#adding cutpoints
abline(v=c(25,40,60),lty=2,col="darkgreen")

bptest(model2)
shapiro.test(residuals(model2)) 


model3 <- lm((Price) ~ poly(Odometer, degree=2, raw=TRUE) , cars[Make=="Saturn",])
summary(model3)
crPlots(model3)



residualPlots(model3)
# Absolute stud. Resid agains fitted  
op <- par(mfrow=c(1,1))
spreadLevelPlot(model3, col="black",pch=".")

bptest(model3)
shapiro.test(residuals(model3)) 


model4 <- lm((Price) ~ poly(Odometer, degree=3, raw=TRUE) , cars[Make=="Saturn",])
bptest(model4)
shapiro.test(residuals(model4)) 

model5=lm(Price ~ bs(Odometer), cars[Make=="Saturn",])
summary(model5)
bptest(model5)
shapiro.test(residuals(model5))


#Q11


cars_new <- data.frame(Odometer = (seq(min(cars$Odometer),max(cars$Odometer),500)))

conf_new  <- predict(model5, newdata = cars_new, interval = "confidence",level = 0.95)
pred_new  <- predict(model5, newdata = cars_new, interval = "prediction",level = 0.95)

plot(Price ~ Odometer, data = cars[Make=="Saturn",], xlim = c(min(Odometer),max(Odometer)), ylim = c(min(Price),max(Price)),pch=20, col = "black", xaxs="i",yaxs="i",
     main="Trees data: Girth and Volume dependence",xlab="Girth", ylab="Volume")
lines(cars_new[,1], pred_new[,1], col='black')
lines(cars_new[,1], pred_new[,2], col='red')
lines(cars_new[,1], pred_new[,3], col='red')
lines(cars_new[,1], conf_new[,2], col='blue')
lines(cars_new[,1], conf_new[,3], col='blue')


#Q12


model6=lm(Price ~ Odometer+Type+Model, cars[Make=="Saturn",])
summary(model6)
bptest(model6)
shapiro.test(residuals(model6))

pairs( ~Price + Odometer+Type+Model,data=cars[Make=="Saturn",],  main="Basic Scatterplot Matrix") 


ggplot(cars[Make=="Saturn",], aes(x=Odometer, y=Price, color=Type)) +
  geom_point() + 
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE)

#Q14

t.test(cars[Cruise==0,]$Price,cars[Cruise==1,]$Price )
t.test(cars[Sound==0,]$Price,cars[Sound==1,]$Price )
t.test(cars[Leather==0,]$Price,cars[Leather==1,]$Price )

#Q15

#modelGM <- lm(Price ~ Type + Odometer + Liter +  Make +Odometer^2
#              ,data=cars) 

modelGM <- lm(Price ~ Type + Odometer + Liter +  Make, data = cars)

model_AIC <-stepAIC(modelGM,k=1)
#model_BIC <-stepAIC(modelGM,k=log(nrow(cars)))
summary(modelGM)
AIC(modelGM)
BIC(modelGM)
plot( modelGM )

#Q16
modelGMlog <- lm(log(Price) ~ Type + Odometer + Liter +  Make + Leather, data = cars)

summary(modelGMlog)
plot(modelGMlog)
bptest(modelGMlog)
shapiro.test(residuals(modelGMlog))


modelbc     <- boxcox(modelGM, lambda = seq(-10,10 , 1/100))
summary(modelbc)
lambda <- modelbc$x[which.max(modelbc$y)]
lambda
modelbox <- lm((Price^lambda-1)/lambda ~  Type + Odometer + Liter +  Make , data = cars)
summary(modelbox)

bptest(modelbox)
shapiro.test(residuals(modelbox))

#Q17

coef(modelGMlog)
coef(model1)

#nezapomenout odtransformovat


#Q18

pairs.panels(cars,smooth=F,lm=T,scale=T,pch=20,cex.cor=4)


round(cor(cars$Odometer, cars$Liter),2)
pairs(cars)
pairs.panels(cars[,2:12],smooth=F,lm=T,scale=T,pch=20,cex.cor=4) #Convertible a Doors, 


pairs.panels(cars[,c(2,3,4,11,12)],smooth=F,lm=T,scale=T,pch=20,cex.cor=4)


vif(modelGMlog)
kappa(scale(cars[,c(2,3,4,11,12)]),exact=T)^2

summary(modelGMlog)




#Q19

crPlots(modelGMlog) #residualni
avPlots(modelGMlog) #regresni

#Q20


#Q21

summary(influence.measures(modelGMlog))
out_index<-as.numeric(row.names(summary(influence.measures(modelGMlog))))
plot(modelGMlog)

out_index

dfbetas(modelGMlog)
dfbetas(modelGMlog)[abs(dfbetas(modelGMlog)[, 2])>1,]

r = sum(hatvalues(modelGMlog)) # rank(X)
n = length(cars)
leverage_level = 3*r/n
sum(hatvalues(modelGMlog)>leverage_level)
cars[hatvalues(modelGMlog)>leverage_level,]

rstudent(modelGMlog)
sort(rstudent(modelGMlog)) #>3 je outlier

#Q22

plot(Odometer,Price)

#dva SAABy s velkym odometerm sedi v trendu, nevyhazuju
#v Price jsou tam convertible, ty ale nemuzu vyhodut --> co ale udelat vlastni model jen pro ne?


carsR <- cars[-c(741,388,382,386, 743, 744), ]
modelGMlogR <- lm(log(Price) ~ Type + Odometer + Liter +  Make, data = carsR)
bptest(modelGMlogR)
shapiro.test(residuals(modelGMlogR))
plot(modelGMlogR)

#Q23


MM_bisquare_stars<- rlm(Price ~ Odometer, method="MM",psi = psi.bisquare) # Tukey
MM_ggw_stars     <- rlm(Price ~ Odometer, method="MM",psi = psi.ggw)      # GGW

coef(MM_bisquare_stars)
coef(MM_ggw_stars)
coef(lm(Price ~ Odometer))

LTS_stars05  <- ltsReg(Price ~ Odometer, alpha=0.5)
LTS_stars09  <- ltsReg(Price ~ Odometer, alpha=0.9)

coef(LTS_stars05)
coef(LTS_stars09)

#Q24

#udelat model zvlast pro convertible
