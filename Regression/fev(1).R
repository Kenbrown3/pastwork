##############################
### Regression Analysis    ###
### PC Session 2: FEV data ###
##############################

# Libraries required: MASS and rgl

rm(list=ls())

# Load fev data
fev <- read.table("fev.txt", header = TRUE)
head(fev)
n <- dim(fev)[1]
p <- dim(fev)[2]
attach(fev)

# Scatter plot
pairs(fev)
pairs(fev, panel = function(x,y) {points(x,y); lines(lowess(x,y), col = "red")})

# Exploratory analysis
summary(fev)
boxplot(FEV)
hist(FEV)

# FEV ~ age + height
fit1 <- lm(FEV ~ age + height, data = fev)
fit1
fit1.sum <- summary(fit1)
fit1.sum

# Check model assumptions
par(mfrow = c(2,2))
plot(fit1)

# Check model assumptions (2)
library(MASS)
fit1.res <- residuals(fit1)
fit1.stdres <- stdres(fit1)
fit1.fittedvalues <- fitted.values(fit1)
par(mfrow = c(2,2))
qqnorm(fit1.stdres, main="")
qqline(fit1.stdres)
plot(fit1.res, xlab = "Index", ylab = "Residual")
plot(fit1.fittedvalues, fit1.res, xlab = "Fitted value", ylab = "Residual")
lines(lowess(fit1.res ~ fit1.fittedvalues), col = "red")
plot(fit1.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-5,5))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)
# UL: deviations from normal distributed residuals at the tails
# UR: pattern indicates heteroscedastic errors
# BL: curved band suggests a different pattern than a linear one
#     variance of residuals increase with the level of the response variable
# BR: several outliers
par(mfrow = c(1,2))
plot(age, fit1.res, ylab = "Residual")
lines(lowess(fit1.res ~ age), col = "red")
plot(height, fit1.res, ylab = "Residual")
lines(lowess(fit1.res ~ height), col = "red")
# plots indicate the linear model is defective (add quadratic terms) and the errors are heteroscedastic

# Partial residual plots
fit1.coef <- coefficients(fit1)
fit1.pres.age <- fit1.res + fit1.coef[2] * age
fit1.pres.height <- fit1.res + fit1.coef[3] * height
par(mfrow = c(1,2))
plot(age, fit1.pres.age, ylab = "Partial residual (age)")
abline(lm(unname(fit1.pres.age) ~ age))
lines(lowess(age, fit1.pres.age), col = "red")
plot(height,fit1.pres.height, ylab = "Partial residual (height)")
abline(lm(fit1.pres.height ~ height))
lines(lowess(height, fit1.pres.height), col = "red")
# plots suggest quadratic terms

# FEV ~ age + age^2 + height + height^2
fit2 <- lm(FEV ~ age + I(age^2) + height + I(height^2), data = fev)
fit2
fit2.sum <- summary(fit2)
fit2.sum
# drop age^2 from the model since P-value = 0.521 > 0.05

# Test significance of squared terms
anova(fit1, fit2)
# equivalently
fit1.res <- residuals(fit1)
fit2.res <- residuals(fit2)
fit1.sse <- sum(fit1.res^2)
fit2.sse <- sum(fit2.res^2)
Fvalue <- ((fit1.sse - fit2.sse)/2)/(fit2.sse/(n - 5))
Fcrit <- qf(0.95, 2, n - 5)
Pvalue <- 1 - pf(Fvalue, 2, n - 5)
# null-hypothesis is rejected since Fvalue = 35.143 > 3.010 = Fcrit or P-value = 0 < 0.05

# FEV ~ age + height + height^2
fit3 <- lm(FEV ~ age + height + I(height^2), data = fev)
fit3
fit3.sum <- summary(fit3)
fit3.sum
# all parameters are significant

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
plot(fit3.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-5,5))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)
# UL: deviations from normal distributed residuals at the tails
# UR: pattern indicates heteroscedastic errors
# BL: funnel pattern indicates errors are heteroscedastic
# BR: several outliers
par(mfrow = c(1,2))
plot(age, fit3.res, ylab = "Residual")
lines(lowess(fit3.res ~ age), col = "red")
plot(height, fit3.res, ylab = "Residual")
lines(lowess(fit3.res ~ height), col = "red")
# plots indicate the linear model has improved but the errors remain heteroscedastic

# FEV ~ age + height + height^2 + age*height
fit4 <- lm(FEV ~ age + height + I(height^2) + age*height, data = fev)
fit4
fit4.sum <- summary(fit4)
fit4.sum
# interaction term is significant since P-value = 0.015 < 0.05
# height^2 can be dropped from the model since P-value = 0.059 > 0.05

# FEV ~ age + height + age*height
fit5 <- lm(FEV ~ age + height + age*height, data = fev)
fit5
fit5.sum <- summary(fit5)
fit5.sum
# for a child age = 0 and height = 0, average FEV = - 0.66 (interpretation makes no sense)
# fix height then a one unit increase of age results in an average FEV increase of - 0.418 + 0.0075 height
# fix age then a one unit increase of height results in an average FEV increase of 0.046 + 0.0075 age

# plot FEV in function of age for fixed height
regsurface <- function(x1,x2) fit5$coef[1] + fit5$coef[2] * x1 + fit5$coef[3] * x2 + fit5$coef[4] * x1 * x2
par(mfrow = c(1,1))
x1 <- seq(0, 20, 1)
x2 <- 50
plot(x1, regsurface(x1, x2), 'l', ylim = c(0,3), xlab = "age", ylab = "FEV")
text(5, 1, "height = 50, FEV =  1.63 - 0.043 * age")
x2 <- 60
lines(x1, regsurface(x1, x2))
text(5, 2.6, "height = 60, FEV =  2.085 + 0.032 * age")

# 3D plot regression surface
library(rgl)
plot3d(age, height, FEV, type = "s", col = "red", size = 1)
x1 <- seq(0, 20)
x2 <- seq(50, 80)
y <- matrix(0, length(x1), length(x2))
for (i in 1:length(x1)) for (j in 1:length(x2)) y[i,j] <- regsurface(x1[i], x2[j])
persp3d(x1, x2, y, alpha = 0.3, add = TRUE)
lines3d(x1, 50, regsurface(x1,50), add = TRUE, col = "red", lwd = 2)
lines3d(x1, 60, regsurface(x1,60), add = TRUE, col = "red", lwd = 2)

detach(fev)
