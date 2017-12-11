data(mtcars)

head(mtcars)

summary(mtcars)

require(stats)
round(cor(mtcars)[-1, 1], 2)
cor(mtcars)[-1, 1]

par(mfrow=c(3,2))
boxplot(mpg ~ am, data = mtcars, main = "Miles per Gallon Vs Transmission "
        , xlab = "Transmission (0-auto, 1-manual)",  ylab = "MPG")
boxplot(mpg ~ cyl, data = mtcars, main = "Miles per Gallon Vs Number of Cylinders "
        , xlab = "Cylinders",  ylab = "MPG")
boxplot(mpg ~ vs, data = mtcars, main = "Miles per Gallon Vs Engine type "
        , xlab = "Engine (0-V enginge, 1-S engine)",  ylab = "MPG")
boxplot(mpg ~ gear, data = mtcars, main = "Miles per Gallon Vs Number of Gears "
        , xlab = "Number of Gears",  ylab = "MPG")
boxplot(mpg ~ carb, data = mtcars, main = "Miles per Gallon Vs Number of Carburetors "
        , xlab = "Number of Carburetors",  ylab = "MPG")

par(mfrow=c(3,2))
plot(mtcars$disp, mtcars$mpg, main = "Miles per Gallon Vs Gas Displacement "
     , xlab = "Displacement (cu.in.)",  ylab = "MPG")
plot(mtcars$hp, mtcars$mpg, main = "Miles per Gallon Vs Gross Horsepower "
     , xlab = "Horsepower",  ylab = "MPG")
plot(mtcars$drat, mtcars$mpg, main = "Miles per Gallon Vs Rear Axle Ratio "
     , xlab = "Rear Axle Ratio",  ylab = "MPG")
plot(mtcars$wt, mtcars$mpg, main = "Miles per Gallon Vs Weight "
     , xlab = "Weight (1000 lbs)",  ylab = "MPG")
plot(mtcars$qsec, mtcars$mpg, main = "Miles per Gallon Vs QSec "
     , xlab = "QSec (1/4 mile time)",  ylab = "MPG")


fit1 <- lm (mpg ~ as.factor(am), data = mtcars)
fit2 <- lm (mpg ~ as.factor(am) + as.factor(cyl) -1 , data = mtcars)
fit3 <- lm (mpg ~ as.factor(am) + as.factor(cyl) + as.factor(vs) , data = mtcars)
fit4 <- lm (mpg ~ as.factor(am) + as.factor(cyl) + as.factor(vs) + as.factor(gear) , data = mtcars)
fit5 <- lm (mpg ~ as.factor(am) + as.factor(cyl) + as.factor(vs) + as.factor(gear) + as.factor(carb) , data = mtcars)
fit6 <- lm (mpg ~ as.factor(am) + as.factor(cyl) + as.factor(vs) + as.factor(gear) + as.factor(carb) + disp , data = mtcars)
fit7 <- lm (mpg ~ as.factor(am) + as.factor(cyl) + as.factor(vs) + as.factor(gear) + as.factor(carb) + disp + hp , data = mtcars)
fit8 <- lm (mpg ~ as.factor(am) + as.factor(cyl) + as.factor(vs) + as.factor(gear) + as.factor(carb) + disp + hp + drat , data = mtcars)
fit9 <- lm (mpg ~ as.factor(am) + as.factor(cyl) + as.factor(vs) + as.factor(gear) + as.factor(carb) + disp + hp + drat + wt , data = mtcars)
fit10 <- lm (mpg ~ as.factor(am) + as.factor(cyl) + as.factor(vs) + as.factor(gear) + as.factor(carb) + disp + hp + drat + wt + qsec , data = mtcars)


summary(fit1)

predict(fit1, am <- 0)

anova(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10)
anova(fit1, fit3, fit5, fit7, fit9)
anova(fit1, fit5, fit9)
anova(fit1)


fit1 <- lm (mpg ~ as.factor(am), data = mtcars)
fit2 <- lm (mpg ~ as.factor(am) + wt -1 , data = mtcars)
fit3 <- lm (mpg ~ as.factor(am) + wt + as.factor(cyl) , data = mtcars)
anova(fit1, fit2, fit3)


summary(fit3)

p <- data.frame(am = as.factor(0), wt = 3.21)
predict(fit2, p)
summary(fit2)
summary(fit1)
summary(fit3)

mean(mtcars$wt)
table(mtcars$cyl)



fit <- lm(mpg ~ wt + cyl, data = mtcars)
summary(fit)


fit2 <- lm(mpg ~ factor(am)+wt+hp+disp+cyl, data = mtcars)
summary(fit2)

fit3 <- lm(mpg ~ factor(am)+wt+hp, data = mtcars)
summary(fit3)

fit4 <- lm(mpg ~ factor(am)+wt+hp, data = mtcars)
summary(fit4)



fit1 <- lm(mpg ~ factor(am) + cyl + disp + wt, data = mtcars)
fit2 <- lm(mpg ~ factor(am) + cyl + disp + wt + hp + drat + vs + carb, data = mtcars)
anova(fit0, fit1, fit2)

plot(fit1)

residuals <- resid(fit1) #List of residuals
predicted <- predict(fit1)
plot(predicted, residuals)

