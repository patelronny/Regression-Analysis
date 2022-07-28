
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

