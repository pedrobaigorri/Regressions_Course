#
# scripts for the resolution of the QUIZZ 3 for Regression Modelling course
#

# QUESTION 1: 
# Consider the mtcars data set. Fit a model with mpg as the outcome that
# includes number of cylinders as a factor variable and weight as confounder. 
# Give the adjusted estimate for the expected change in mpg comparing 8 cylinders to 4.

data(mtcars)
y <- mtcars$mpg
x1 <- as.factor(mtcars$cyl)
x2 <- mtcars$wt

fit <- lm(y ~ x1 + x2)

levels(x1)

fit$coefficients[3]

# QUESTION 2:
# Consider the mtcars data set. Fit a model with mpg as the outcome that includes
# number of cylinders as a factor variable and weight as a possible confounding variable. 
# Compare the effect of 8 versus 4 cylinders on mpg for the adjusted and unadjusted by weight models. 
# Here, adjusted means including the weight variable as a term in the regression model 
# and unadjusted means the model without weight included. 
# What can be said about the effect comparing 8 and 4 cylinders after looking at models with and without weight included?.

fit_adjusted <- lm(y ~ x1 + x2)
fit_unadjusted <- lm(y ~ x1)

fit_adjusted$coefficients
fit_unadjusted$coefficients

#solution: Holding weight constant, cylinder appears to have less of an impact on mpg than if weight is disregarded.


# QUESTION 3:
# Consider the mtcars data set. Fit a model with mpg as the outcome that considers 
# number of cylinders as a factor variable and weight as confounder. 
# Now fit a second model with mpg as the outcome model that considers the 
# interaction between number of cylinders (as a factor variable) and weight. 
# Give the P-value for the likelihood ratio test comparing the two models and 
# suggest a model using 0.05 as a type I error rate significance benchmark.
data(mtcars)
y <- mtcars$mpg
x1 <- as.factor(mtcars$cyl)
x2 <- mtcars$wt


fit1 <- lm(y ~ x1 + x2)
fit2 <- lm(y ~ x1 * x2)

summary(fit1)$coef
summary(fit2)$coef

anova(fit1, fit2)

# Solution: The P-value is larger than 0.05. So, according to our criterion, 
#we would fail to reject, which suggests that the interaction terms may not be necessary.

# QUESTION 4:
# Consider the mtcars data set. Fit a model with mpg as the outcome that includes 
# number of cylinders as a factor variable and weight inlcuded in the model as

fit <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)

# How is the wt coefficient interpretted?
fit$coefficients

#NOOO #solution: The estimated expected change in MPG per half ton increase in weight for the average number of cylinders.

# QUESTION 5:
# Consider the following data set
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

# Give the hat diagonal for the most influential point  
influence(lm(y ~ x))$hat


# QUESTION 6:
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
# Give the slope dfbeta for the point with the highest hat value.
inf <- influence(lm(y ~ x))
inf$hat
dfbetas(lm(y ~ x))[5,2]


#QUESTION 7:


#solution:
# It is possible for the coefficient to reverse sign after adjustment. 
# For example, it can be strongly significant and positive before adjustment and
# strongly significant and negative after adjustment.
