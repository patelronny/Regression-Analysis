
library(MASS)

plot(cats$Bwt,cats$Hwt)

str(cats)
View(cats)

cor(cats$Bwt,cats$Hwt)

cor(cats$Hwt,cats$Bwt)

plot(density(cats$Hwt))

sum(is.na(cats$Bwt))

sum(is.na(cats$Hwt))

boxplot(cats$Bwt,cats$Hwt)

boxplot.stats(cats$Hwt)$out

median(cats$Hwt)

newcats2 <- cats
str(newcats2)

newcats2[newcats2$Hwt%in%c(17.2,20.5),3] = median(newcats2$Hwt)

boxplot(newcats2$Bwt,newcats2$Hwt)

boxplot.stats(newcats2$Hwt)$out

cats_model <- lm(Hwt~Bwt, data = newcats2)
cats_model

cor(newcats2$Bwt,newcats2$Hwt)

sd(newcats2$Hwt)
sd(newcats2$Bwt)

b1 = cor(newcats2$Bwt,newcats2$Hwt)*(sd(newcats2$Hwt)/sd(newcats2$Bwt))
b1

b0 = mean(newcats2$Hwt)-(mean(newcats2$Bwt)*b1)
b0

summary(cats_model)

cor(newcats2$Hwt,newcats2$Bwt)
cov(newcats2$Hwt,newcats2$Bwt)



# Residual
  in residual section check the median value, this value has to be closure to 0.
  closure to 0 better the the model
  
# coefficient
  in coefficients we check significance of each factor, t value in this section shuld
  be less than 0.05
  
# estimate
  used to derive 
  
  standard error to compute confidence interval
  mean(x)+/-2*
    
RMSE
  is an indicator of linear model smaller the value better the model
  RMSE =sqrt of average of 
R2 here means 58.72% variation in hwt explained by bwt

p value should be less than derived equation value.


plot(newcats2$Bwt,newcats2$Hwt,main = "Regressionlinebwtvshwt",
     xlab = "bodyweight,Bwt",
     ylab = "heart weight,Hwt")

abline(cats_model,col="green")

plot(newcats2$Bwt, cats_model$residual, main = "homoscedasticity, evsbwt",
     xlab = "bodyweight, Bwt",
     ylab = "error,e")
abline(lm(cats_model$residual~newcats2$Bwt), col="red")

newBwt <- data.frame(Bwt = c(2.3,2.35,2.4,2.45,2.5))
  
newBwt

newhwt <- predict(cats_model,newdata = newBwt)
newhwt

predicted_Hwt <- cbind(newBwt,newhwt)
predicted_Hwt

population1 <- data.frame(year = 2001:2010, 
                          population = c(10.2,10.4,10.5,10.6,10.8,10.10,11.1,11.3,11.4,11.6))

population1

cor(population1$year,population1$population)

plot(density(population1$population))

boxplot(population1$year,population1$population)

boxplot.stats(population1$year)$out
boxplot.stats(population1$population)$out
median(population1$population)

pop_model <- lm(population~year, data = population1)
pop_model


cor(population1$year,population1$population)

sd(population1$population)
sd(population1$year)

b1 = cor(population1$year,population1$population)*(sd(population1$population)/sd(population1$year))
b1

b0 = mean(population1$population)-(mean(population1$year)*b1)
b0

summary(pop_model)

plot(population1$year,population1$population,main = "Regressionlineyearvspop",
     xlab = "year,year",
     ylab = "population,population")

abline(pop_model,col="green")

plot(population1$year, pop_model$residual, main = "homoscedasticity, evsyear",
     xlab = "year, year",
     ylab = "error,e")
abline(lm(pop_model$residual~population1$year), col="red")

newyear <- data.frame(year = 2011:2015)

newyear

newpopu <- predict(pop_model,newdata = newyear)
newpopu

predicted_popu <- cbind(newyear,newpopu)
predicted_popu

plot(pop_model,main = "Regressionlinearvsyear",xlab = "newyear,newyear",ylab = "newpopu,newpopu")
abline(pop_model,col="green")
  
setwd("G:/R Tools/R 2nd part/assignments/Case Study Regression")
getwd()

rail_passengers <- read.table("train.txt");View(rail_passengers)

rail_passengers2 <- data.frame(cars = rail_passengers$V2,
                      passengers = rail_passengers$V4)
View(rail_passengers2)


cor(rail_passengers2$cars,rail_passengers2$passengers)

plot(density(rail_passengers2$cars))

boxplot(rail_passengers2$passengers,rail_passengers2$cars)

boxplot.stats(rail_passengers2$passengers)$out

median(cats$Hwt)

cars_model <- lm(cars~passengers, data = rail_passengers2)
cars_model


cor(population1$year,population1$population)

sd(population1$population)
sd(population1$year)

b1 = cor(population1$year,population1$population)*(sd(population1$population)/sd(population1$year))
b1

b0 = mean(population1$population)-(mean(population1$year)*b1)
b0

summary(pop_model)

plot(population1$year,population1$population,main = "Regressionlineyearvspop",
     xlab = "year,year",
     ylab = "population,population")

abline(pop_model,col="green")

plot(population1$year, pop_model$residual, main = "homoscedasticity, evsyear",
     xlab = "year, year",
     ylab = "error,e")
abline(lm(pop_model$residual~population1$year), col="red")

newyear <- data.frame(year = 2011:2015)

newyear

newpopu <- predict(pop_model,newdata = newyear)
newpopu

predicted_popu <- cbind(newyear,newpopu)
predicted_popu

