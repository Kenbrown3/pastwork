######################################################
### Regression Analysis                            ###
### PC session 3: Standard metropolitan areas data ###
######################################################

# Libraries: MASS

rm(list = ls())

# Load standard metropolitan areas data
# Variables: Land area, Total population, Percent city, Percent senior, Physicians, Hospital beds, Graduates, Labor, Income, Crimes, Region
sma <- read.table("sma.txt", row.names = 1)
names(sma) <- c('LA', 'TP', 'PC', 'PS', 'P', 'HB', 'G', 'L', 'I', 'C', 'R')
head(sma)
summary(sma)

# Construct response
sma$CR <- sma$C/sma$TP
summary(sma$CR)
hist(sma$CR, xlab = "Crime rate", main = "Histogram of crime rate")
boxplot(sma$CR)

# Training and validation set
n <- dim(sma)[1]
sma.tr <- sma[seq(2, n, by = 2),]
sma.val <- sma[seq(1, n, by = 2),]
attach(sma.tr)

#-----------------#
# Model Selection #
#-----------------#

# Backward elimination based on AIC
fit.full <- lm(CR ~ LA + PC + PS + P + HB + G + L + I, data = sma.tr)
fit.full
library(MASS)
stepAIC(fit.full, scope = list(upper = ~ LA + PC + PS + P + HB + G + L + I, lower = ~ 1), direction = "backward")
# AIC = 355.93
# CR ~ LA + PC + HB + G + I

# Forward selection based on AIC
fit.null <- lm(CR ~ 1, data = sma.tr)
fit.null
stepAIC(fit.null, scope = list(upper = ~ LA + PC + PS + P + HB + G + L + I, lower = ~ 1), direction = "forward")
# AIC = 357.75
# CR ~ LA + G

# Stepwise selection based on AIC (started at full model)
stepAIC(fit.full, scope=list(upper = ~ LA + PC + PS + P + HB + G + L + I, lower = ~ 1), direction = "both")
# AIC = 355.93
# CR ~ LA + PC + HB + G + I

# Stepwise selection based on AIC (started at null model)
stepAIC(fit.null, scope=list(upper = ~ LA + PC + PS + P + HB + G + L + I, lower = ~ 1), direction = "both")
# AIC = 357.75
# CR ~ LA + G

# Backward elimination based on F-statistic/t-statistic
dropterm(fit.full, test = "F")
# Remove PS
fit1 <- update(fit.full, ~ . - PS)
dropterm(fit1, test = "F")
# Remove P
fit2 <- update(fit1, ~ . - P)
dropterm(fit2, test = "F")
# Remove L
fit3 <- update(fit2, ~ . - L)
dropterm(fit3, test = "F")
# Remove PC
fit4 <- update(fit3, ~ . - PC)
dropterm(fit4, test = "F")
# CR ~ LA + HB + G + I

# Forward selection based on F-statistic/t-statistic
addterm(fit.null, ~ . + LA + PC + PS + P + HB + G + L + I, test = "F")
# Add G
fit1 <- update(fit.null, ~ . + G)
addterm(fit1, ~ . + LA + PC + PS + P + HB + L + I, test = "F")
# Add LA
fit2 <- update(fit1, ~ . + LA)
addterm(fit2, ~. + PC + PS + P + HB + L + I, test = "F")
# CR ~ LA + G

# Model 1: CR ~ LA + PC + HB + G + I
model1 <- lm(CR ~ LA + PC + HB + G + I, data = sma.tr)
# Model 2:  CR ~ LA + G
model2 <- lm(CR ~ LA + G, data = sma.tr)
# Model 3: CR ~ LA + HB + G + I
model3 <- lm(CR ~ LA + HB + G + I, data = sma.tr)

#------------------#
# Model validation #
#------------------#

# PRESS
PRESS1 <- sum((residuals(model1) / (1 - lm.influence(model1)$hat))^2)
PRESS2 <- sum((residuals(model2) / (1 - lm.influence(model2)$hat))^2)
PRESS3 <- sum((residuals(model3) / (1 - lm.influence(model3)$hat))^2)
PRESS <- c(PRESS1, PRESS2, PRESS3)
names(PRESS) <- c("model1", "model2", "model3")
sort(PRESS)

#  Compare estimated coefficients and standard errors
detach(sma.tr)
attach(sma.val)
# Model 1: CR ~ LA + PC + HB + G + I
model1.val <- lm(CR ~ LA + PC + HB + G + I, data = sma.val)
summary(model1.val)
summary(model1)
# Model 2: CR ~ LA + G
model2.val <- lm(CR ~ LA + G, data = sma.val)
summary(model2.val)
summary(model2)
# Model 3: CR ~ LA + HB + G + I
model3.val <- lm(CR ~ LA + HB + G + I, data = sma.val) 
summary(model3.val)
summary(model3)
# Model 2 gives the best resemblance between the coefficients and standard errors

# MSE
MSE1 <- summary(model1)$sigma^2
MSE2 <- summary(model2)$sigma^2
MSE3 <- summary(model3)$sigma^2
MSE <- c(MSE1, MSE2, MSE3)
names(MSE) <- c("model1", "model2", "model3")
sort(MSE)

# MSEP
MSEP1 <- mean((predict(model1, newdata = sma.val) - CR)^2)
MSEP2 <- mean((predict(model2, newdata = sma.val) - CR)^2)
MSEP3 <- mean((predict(model3, newdata = sma.val) - CR)^2)
MSEP <- c(MSEP1, MSEP2, MSEP3)
names(MSEP) <- c("model1", "model2", "model3")
sort(MSEP)

# Results
validation.results <- data.frame(rbind(PRESS/n, MSE, MSEP), row.names = c("PRESS/n", "MSE", "MSEP"))
names(validation.results) <- c("model1", "model2", "model3")
validation.results
# Model 1 is the optimal model with respect to PRESS/n, MSE and MSEP

detach(sma.val)
