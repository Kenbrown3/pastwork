###############################
### Regression Analysis     ###
### PC Session 2: Cars data ###
###############################

# Libraries required: MASS

rm(list = ls())

# Load cars data
cars <- read.csv(choose.files(), header = TRUE)
cars <- cars[,-c(1,6)]
cars
n <- dim(cars)[1]
p <- dim(cars)[2]
attach(cars)

# Scatter plot
pairs(cars)
pairs(cars, panel = function(x,y) {points(x,y); lines(lowess(x,y), col = "red")})

# MPG ~ Weight + Cylinders + Horsepower
fit1 <- lm(MPG ~ Weight + Cylinders + Horsepower, data = cars)
fit1
fit1.sum <- summary(fit1)
fit1.sum
# Cylinders has a positive effect on MPG while it should be negative
# Cylinders can be removed from the model since P-value = 0.311 > 0.05
# Horsepower has a negative effect on MPG, hence the claim is satisfied
# Horsepower can be removed from the model since P-value = 0.104 > 0.05

# Correlation between predictors
cor(cars[,2:p])
# correlation between predictors is large
# this could explain the "wrong" signs of the regression coefficients

# Hypothesis test
tvalue <- (fit1.sum$coefficients[2,1] - ( - 8)) / fit1.sum$coefficients[2,2]
tcrit <- qt(0.975, n - p)
Pvalue <- 2*(1 - pt(abs(tvalue), n - p))
# null-hypothesis can not be rejected since tvalue = 0.293 < 2.032 = tcrit or P-value = 0.771 > 0.05

# MPG ~ Weight
fit2 <- lm(MPG ~ Weight, data = cars)
fit2
fit2.sum <- summary(fit2)
fit2.sum

# Plot of regression line
plot(Weight, MPG)
abline(fit2, col = "red")
# relation between MPG and Weight is curved

# Check model assumptions
par(mfrow = c(2,2))
plot(fit2)

# Check model assumptions (2)
library(MASS)
fit2.res <- residuals(fit2)
fit2.stdres <- stdres(fit2)
fit2.fittedvalues <- fitted.values(fit2)
par(mfrow = c(2,2))
qqnorm(fit2.stdres, main="")
qqline(fit2.stdres)
plot(fit2.res, xlab = "Index", ylab = "Residual")
plot(fit2.fittedvalues, fit2.res, xlab = "Fitted value", ylab = "Residual")
lines(lowess(fit2.res ~ fit2.fittedvalues), col = "red")
plot(fit2.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)
# UL: deviations from normal distributed residuals
# UR: parabolic trend suggests adding first-order or second-order terms
# BL: curved band suggests a different pattern than a linear one
# BR: no outliers
par(mfrow = c(1,1))
plot(Weight, fit2.res, ylab = "Residual")
lines(lowess(fit2.res ~ Weight), col = "red")
# plot indicates the linear model is defective

# MPG ~ Weight + Weight^2
fit3 <- lm(MPG ~ Weight + I(Weight^2), data = cars)
fit3
fit3.sum <- summary(fit3)
fit3.sum
# Weight^2 is contributing to the fit since P-value = 0.0021 < 0.05

# Plot regression curve
plot(Weight, MPG)
abline(fit2, col = "red")
regline <- function(x) return(fit3$coefficients[1] + fit3$coefficients[2] * x + fit3$coefficients[3] * x^2)
x <- seq(0, 5, by = 0.1)
lines(x, regline(x), col = "blue")
legend(3.5, 35, c("linear model", "quadratic model"), lty = 1, col = c("red", "blue"))
# or
plot(Weight, MPG)
lines(x, fit3$coefficients[1] + fit3$coefficients[2] * x + fit3$coefficients[3] * x^2, col = "blue")
# or
plot(Weight, MPG)
curve(fit3$coefficients[1] + fit3$coefficients[2] * x + fit3$coefficients[3] * x^2, 0, 5, add = TRUE, col = "blue")
# or
plot(Weight, MPG)
lines(sort(Weight), fit3$fitted.values[order(Weight)], col = "blue")
# regression curve follows the main trend better

# Check model assumptions
par(mfrow = c(2,2))
plot(fit3)

# Check model assumptions (2)
fit3.res <- residuals(fit3)
fit3.stdres <- stdres(fit3)
fit3.fittedvalues <- fitted.values(fit3)
par(mfrow = c(2,2))
qqnorm(fit3.stdres, main="")
qqline(fit3.stdres)
plot(fit3.res, xlab = "Index", ylab = "Residual")
plot(fit3.fittedvalues, fit3.res, xlab = "Fitted value", ylab = "Residual")
lines(lowess(fit3.res ~ fit3.fittedvalues), col = "red")
plot(fit3.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)
# UL: small deviations from normal distributed residuals at the right tail
# UR: still parabolic trend
# BL: curved band remains but curvature is smaller
# BR: one outlier
par(mfrow = c(1,1))
plot(Weight, fit3.res, ylab = "Residual")
lines(lowess(fit3.res ~ Weight), col = "red")
# parabolic behavior is reduced

# Scale predictor
Weight.mean <- mean(Weight)
cars2 <- data.frame(MPG, Weight.centered = Weight - Weight.mean)
detach(cars)
attach(cars2)

# MPG ~ Weight.centered + Weight.centered^2
fit4 <- lm(MPG ~ Weight.centered + I(Weight.centered^2), data = cars2)
fit4
fit4.sum <- summary(fit4)
fit4.sum

# Comparison of regression coefficients of fit3 and fit4 (see p63 of course notes)
fit3.coef <- unname(coefficients(fit3))
fit4.coef <- unname(coefficients(fit4))
# beta_2 = beta'_2
fit3.coef[3]
fit4.coef[3]
# beta_1 = beta'_1 - 2 * beta'2 * mean(Weight)
fit3.coef[2]
fit4.coef[2] - 2 * fit4.coef[3] * Weight.mean
# beta_0 = beta'_0 - beta'_1 * mean(Weight) + beta'_2 * mean(Weight)^2
fit3.coef[1]
fit4.coef[1] - fit4.coef[2] * Weight.mean + fit4.coef[3] * Weight.mean^2

detach(cars2)
