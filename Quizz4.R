#
# Scripts for the resolution of the QUIZZ 4 for Regression Modelling course
#

# QUESTION 1
#
# Consider the space shuttle data ?shuttle in the MASS library. Consider
# modeling the use of the autolander as the outcome (variable name use). 
# Fit a logistic regression model with autolander (variable auto) use
# (labeled as "auto" 1) versus not (0) as predicted by wind sign (variable wind). 
# Give the estimated odds ratio for autolander use comparing head winds, 
# labeled as "head" in the variable headwind (numerator) to tail winds (denominator).

library(MASS)

data(shuttle)

head(shuttle)

shuttle$auto_bin <- as.factor(ifelse(shuttle$use == "auto", 1, 0))

table(shuttle$wind)


fit <- glm(auto_bin ~ wind, data = shuttle, family = "binomial")


summary(fit)

exp(fit$coef[1])/exp(fit$coef[1] +fit$coef[2])

#another way

shuttle$auto <- 1 * (shuttle$use == "auto")
shuttle$headwind <- 1 * (shuttle$wind == "head")
fit <- glm(auto ~ headwind, data = shuttle, family = binomial)
exp(coef(fit))


exp(coef(fit))

# QUESTION 2
#
# Consider the previous problem. Give the estimated odds ratio for autolander
# use comparing head winds (numerator) to tail winds (denominator) adjusting for
# wind strength from the variable magn.

shuttle$auto <- 1 * (shuttle$use == "auto")
shuttle$headwind <- 1 * (shuttle$wind == "head")
fit <- glm(auto ~ headwind + as.factor(magn), data = shuttle, family = binomial)
exp(coef(fit))

table(shuttle$magn)


#another way
fit <- glm(auto_bin ~ wind + magn, data = shuttle, family = "binomial")

summary(fit)

exp(fit$coef[1])/exp(fit$coef[1] +fit$coef[2])


# QUESTION 3
#
# If you fit a logistic regression model to a binary variable, for example use
# of the autolander, then fit a logistic regression model for one minus the outcome (not using the autolander) 
# what happens to the coefficients?

shuttle$auto_bin <- -1 *(ifelse(shuttle$use == "auto", 1, 0) - 1)
shuttle$headwind <- 1 * (shuttle$wind == "head")
fit <- glm(auto_bin ~ headwind, data = shuttle, family = "binomial")
summary(fit)

shuttle$auto_bin <- (ifelse(shuttle$use == "auto", 1, 0))
shuttle$headwind <- 1 * (shuttle$wind == "head")
fit <- glm(auto_bin ~ headwind, data = shuttle, family = "binomial")
summary(fit)


# QUESTION 4
#
# Consider the insect spray data InsectSprays. Fit a Poisson model using 
# spray as a factor level. Report the estimated relative rate comapring spray A 
# (numerator) to spray B (denominator).

fit <- glm(count ~ relevel(spray, "B"), data = InsectSprays, family = poisson)
summary(fit)
exp(coef(fit))[2]

#another way 
fit <- glm(count ~ spray, data = InsectSprays, family = poisson)
summary(fit)
exp(fit$coef[1])/exp(fit$coef[1] +fit$coef[2])

# QUESTION 5
#
# Consider a Poisson glm with an offset, t. So, for example, a model of the 
# form glm(count ~ x + offset(t), family = poisson) where x is a factor variable 
# comparing a treatment (1) to a control (0) and t is the natural log of a monitoring time. 
# What is impact of the coefficient for x if we fit the model glm(count ~ x + offset(t2), family = poisson) 
# where 2 <- log(10) + t? In other words, what happens to the coefficients if we 
# change the units of the offset variable. (Note, adding log(10) on the log scale is multiplying by 10 on the original scale.)

# SOLUTION: multiplied by 10

# QUESTION 6
#
# Consider the data

x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

# Using a knot point at 0, fit a linear model that looks like a hockey stick 
# with two lines meeting at x=0. Include an intercept term, x and the knot point term. 
# What is the estimated slope of the line after 0?

plot(x, y)


z <- (x > 0) * x
fit <- lm(y ~ x + z)
summary(fit)
sum(coef(fit)[2:3])
