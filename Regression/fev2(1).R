##############################
### Regression Analysis    ###
### PC Session 3: FEV data ###
##############################

# Libraries required:

rm(list=ls())

# Load fev data
# sex is coded 0 for females and 1 for males
# smoke is coded 0 for non-smokers and 1 for smokers
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
sum(sex)
sum(smoke)
boxplot(FEV[which(smoke == 0)], FEV[which(smoke == 1)], names = c("Non-smokers (n = 589)", "Smokers (n = 65)"), ylab = "FEV")
boxplot(age[which(smoke == 0)], age[which(smoke == 1)], names = c("Non-smokers (n = 589)", "Smokers (n = 65)"), ylab = "age")
boxplot(height[which(smoke == 0)], height[which(smoke == 1)], names = c("Non-smokers (589)", "Smokers (65)"), ylab = "height")
# FEV seems larger for smokers than for non-smokers

# Plot of FEV vs age for non-smokers and smokers
plot(age[which(smoke == 0)], FEV[which(smoke == 0)], xlab = 'age', ylab = 'FEV', col = "black")
points(age[which(smoke == 1)], FEV[which(smoke == 1)], col = 'blue')
legend(3, 5.5, c("Non-smokers", "Smokers"), pch = 1, col = c("black", "blue"))
# FEV increases if age increases, but smokers seem to have lower FEV compared with non-smokers of the same age
# this conclusion contrasts with the boxplots of before
# one regression line for non-smokers and one parallel regression line for smokers seems appropriate

# FEV ~ age
fit1 <- lm(FEV ~ age, data = fev)
fit1
fit1.sum <- summary(fit1)
fit1.sum
# intercept: average FEV for a child of age 0 is 0.432
# slope: if age is increased one unit, the average FEV content increases 0.222 units
abline(fit1, col = "red")

# FEV ~ age + smoke
fit2 <- lm(FEV ~ age + smoke, data = fev)
fit2
fit2.sum <- summary(fit2)
fit2.sum
fit2.coef <- coefficients(fit2)
# for smokers the average FEV is 0.209 lower than the average FEV for non-smokers of the same age
abline(a = fit2.coef[1], b = fit2.coef[2], col = 'green')
abline(a = fit2.coef[1] + fit2.coef[3], b = fit2.coef[2], col = 'green')

# FEV ~ age + smoke + age * smoke
fit3 <- lm(FEV ~ age + smoke + age * smoke, data = fev)
fit3
fit3.sum <- summary(fit3)
fit3.sum
# for non-smokers: FEV = 0.253 + 0.243 * age
# for smokers: FEV = 0.253 + 1.943 + (0.243 - 0.163) * age
fit3.coef <- coefficients(fit3)
abline(a = fit3.coef[1], b = fit3.coef[2], col = 'purple')
abline(a = fit3.coef[1] + fit3.coef[3], b = fit3.coef[2] + fit3.coef[4], col = 'purple')
legend(3, 4.5, c("linear model (without smoke)", "linear model (with smoke)", "interaction model"), lty = 1, col = c("red", "green", "purple"))

# FEV ~ age + height + height^2 + sex + smoke
fit4 <- lm(FEV ~ age + height + I(height^2) + sex + smoke, data = fev)
fit4
fit4.sum <- summary(fit4)
fit4.sum
# all parameters are significant
# for males the average FEV is 0.095 larger than the average FEV for females of the same age and smoking status

# Mean response confidence interval
preddata <- matrix(c(10, 50, 1, 0), ncol = 4)
preddata <- as.data.frame(preddata)
names(preddata) <- c("age", "height", "sex", "smoke")
predict(fit4, newdata = preddata, interval = 'confidence') # mean response
# equivalently
fit4.coef <- coefficients(fit4)
pred <- fit4.coef[1] + fit4.coef[2] * 10 + fit4.coef[3] * 50 + fit4.coef[4] * 50^2 + fit4.coef[5]
pred <- unname(pred)
X <- model.matrix(fit4)
x_0 <- matrix(c(1, 10, 50, 50^2, 1, 0), ncol = 1)
half <- qt(0.025, n - 6) * fit4.sum$sigma * sqrt(t(x_0) %*% solve(t(X) %*% X) %*% x_0)
half <- as.numeric(half)
pred + half
pred - half
# CI = [1.66, 1.91]

# Prediction confidence interval
preddata <- matrix(c(16, 70, 0, 1), ncol = 4)
preddata <- as.data.frame(preddata)
names(preddata) <- c("age", "height", "sex", "smoke")
predict(fit4, newdata = preddata, interval = 'prediction') # just response (not mean response)
# equivalently
pred <- fit4.coef[1] + fit4.coef[2] * 16 + fit4.coef[3] * 70 + fit4.coef[4] * 70^2 + fit4.coef[6]
pred <- unname(pred)
x_0 <- matrix(c(1, 16, 70, 70^2, 0, 1), ncol = 1)
half <- qt(0.025, n - 6) * fit4.sum$sigma * sqrt(t(x_0) %*% solve(t(X) %*% X) %*% x_0 + 1)
half <- as.numeric(half)
pred + half
pred - half
# CI = [3.20, 4.77]

detach(fev)
