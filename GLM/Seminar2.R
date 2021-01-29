#####################################
####### title: "Seminar 2 - GLM" ####
####### author: "Group #01"      ####    
####### date: "03/17/2020"       ####
#####################################

rm(list = ls())

#import the data
victim = read.delim("C:/Users/kebro/OneDrive/KU_Leuven/GLM/victim.txt")
View(victim)
victim$race1 = ifelse(victim$race=="black",0,1)

#Check the missing value(s) 
cat("Number of missing values per factor:\n")
sapply(victim, function(x) sum(is.na(x))) #No missing values

#Basic information about the data
dim(victim)
str(victim)
summary(victim)
attach(victim)

###################################################
################ 1. data exploration ##############
###################################################

library(gplots)
boxplot(resp~race, horizontal=F, col = "gray", data = victim,main='number of victims by race')
plotmeans(resp~race,xlab="race",ylab="resp", main="Mean plot with 95% CI", data=victim)

#Means and variances
library(plyr)
means.var=ddply(victim,~race,summarise,mean=mean(resp), var=var(resp))
means.var


##################################################
############ 2. Poisson model ####################
##################################################

sem2.po<-glm(resp~race, family=poisson(link = "log"), data=victim)
summary(sem2.po)

## 2.1 calculate risk ratio and the corresponding confidence interval
## the average number of victims the white knows is 0.177 the average number of victims the black group knows)

# Create rate ratio calculation function 
glm.RR <- function(GLM.RESULT, digits = 2) {
  if (GLM.RESULT$family$family == "binomial") {
    LABEL = "OR"
  } else if (GLM.RESULT$family$family == "poisson") {
    LABEL = "RR"
  } else {
    stop("Not logistic or Poisson model")
  }
  COEF      = stats::coef(GLM.RESULT)
  CONFINT   = stats::confint(GLM.RESULT)
  TABLE     = cbind(coef=COEF, CONFINT)
  TABLE.EXP = round(exp(TABLE), digits)
  colnames(TABLE.EXP)[1] = LABEL
  TABLE.EXP
}

# Calculate the risk ratio and the corresponding confidence interval 
glm.RR(sem2.po, 3)

# 2.2 Calculate the ratio of the means of the response for each race
# the average number of victims the black knows is 5.6584 times larger the average number of victims the white group knows

# mean response for black 
b=exp(sem2.po$coefficients[1])    
# mean response for white 
w=exp(sem2.po$coefficients[1]+sem2.po$coefficients[2])  
# means response ratio 5.658419  
b/w  

# 2.3 Calculate the predictions of the models for each race 

## Predict case per person (n = 1) for white people 
exp(predict(sem2.po, newdata = data.frame(race = "white", n = 1)))
## Predict case per person (n = 1) for black people 
exp(predict(sem2.po, newdata = data.frame(race = "black", n = 1)))

# 2.4 Analyze the GOF of the model 

# Pearson test p =0
X2.po=sum(residuals(sem2.po, type = "pearson")^2)
n=dim(victim)[1]
p=length(coef(sem2.po))
data.frame(X2s=X2.po,pvalue=(1-pchisq(X2.po,n-p)))

#  deviance test P value=1
Dev.po=summary(sem2.po)$deviance
df.po=summary(sem2.po)$df.residual
data.frame(Dev=Dev.po, df=df.po, pvalue=(1-pchisq(Dev.po,df.po)))

# overdispersion test
library(AER)
dispersiontest(sem2.po)                                                           

library(DHARMa)                                                           
sim.model = simulateResiduals(sem2.po, refit=T)
testDispersion(sim.model) 

#rootogram to visualize the fit of Poisson regression model
install.packages("countreg", repos="http://R-Forge.R-project.org")
library(countreg)
rootogram(sem2.po)

##################################################
############ 3. Negative binomial model###########
##################################################

# Negative binomial regression (estimates the same, se is larger,AIC is smaller)
library(MASS)
sem2.nb <- glm.nb(resp~race, data = victim)
summary(sem2.nb)
 
## GOF test
# Pearson test p =0.01202883
X2.po.nb=sum(residuals(sem2.nb, type = "pearson")^2)
n=dim(victim)[1]
p=length(coef(sem2.nb))
data.frame(X2s=X2.po.nb,pvalue=(1-pchisq(X2.po.nb,n-p)))

##  deviance test P value=1
Dev.po.nb=summary(sem2.nb)$deviance
df.po.nb=summary(sem2.nb)$df.residual
data.frame(Dev=Dev.po.nb, df=df.po.nb, pvalue=(1-pchisq(Dev.po.nb,df.po.nb)))

## Expected number of victims for white
mean_white = exp(sem2.nb$coefficients[[1]]+sem2.nb$coefficients[[2]]*1)
# Variance of victims for white
var_white = mean_white + (1/sem2.nb$theta)*mean_white^2

# Expected number of victims for black
mean_black = exp(sem2.nb$coefficients[[1]]+sem2.nb$coefficients[[2]]*0)
# Variance of victims for black
var_black = mean_black + (1/sem2.nb$theta)*mean_black^2

# observed variance
victim1=victim[which(victim$race=='white'),]
victim2=victim[which(victim$race=='black'),]

c(obsw=var(victim1$resp),obsb=var(victim2$resp), 
  poiwhite=var_white,poiblack=var_black)

#rootogram to visualize the fit of Negative binomial model
rootogram(sem2.nb)

##################################################
############ 4. Quasi-likelihood model############
##################################################

sem2.q = glm(resp~race, family=quasipoisson, data = victim)
summary(sem2.q)
summary(sem2.po)

## GOF test
# Pearson test p =0
X2.po.q=sum(residuals(sem2.q, type = "pearson")^2)
n=dim(victim)[1]
p=length(coef(sem2.q))
data.frame(X2s=X2.po.q,pvalue=(1-pchisq(X2.po.q,n-p)))

## zero inflated model(not necessary)
## In the case 91% of responds in the sample know no victims but the Poisson model predicts that only 87% of them don't know
zobs = victim$resp == 0
zpoi = exp(-exp(predict(sem2.po))) 
c(obs=mean(zobs), poi=mean(zpoi))

##################################################
############ 5. Discuss all the results###########
##################################################

# compare estimates and standard errors of diffrent methods 
round(data.frame(
  Po=coef(sem2.po),QL=coef(sem2.q),NB=coef(sem2.nb),
  se.Po=summary(sem2.po)$coefficients[, 2],
  se.QL=summary(sem2.q)$coefficients[, 2],
  se.NB=summary(sem2.nb)$coefficients[, 2]),4)


# use negative binomial parameters to simulate data and compare the simulated data to the observed data
library(magrittr) 
op = par(mfrow=c(1,2))
set.seed(1)

victim$resp %>% `[`(victim$race=="white") %>% 
  table() %>% barplot(main = "Observed White")
rnbinom(n = 1149, size = sem2.nb$theta, mu = exp(coef(sem2.nb)[1])) %>% 
  table() %>%  barplot(main = "Simulated White")

victim$resp %>% `[`(victim$race=="black") %>% 
  table() %>% barplot(main = "Observed Black")
rnbinom(n = 159, size = sem2.nb$theta, mu = exp(sum(coef(sem2.nb)))) %>% 
  table() %>%  barplot(main = "Simulated Black")
par(op)
