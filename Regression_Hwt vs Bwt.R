# Prectiction for Cats Body weight vs. Heart weight value

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

View(newcats2)

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



## Residual :-
# in residual section check the median value, this value has to be closure to 0.
# closure to 0 better the the model

## coefficient :-
# in coefficients we check significance of each factor, t value in this section shuld be less than 0.05

## estimate :-
# used to derive standard error to compute confidence interval
# mean(x)+/-2*

## RMSE :-
# is an indicator of linear model smaller the value better the model
# RMSE =sqrt of average of R2 here means 58.72% variation in hwt explained by bwt

# p value should be less than derived equation value.


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
