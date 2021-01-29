#=========================#
# Linear Models Project   #
# December 2020           #
# Team 07                 #
#=========================#
rm(list = ls())
par()


## Libraries
library(rio)
library(tidyverse)
library(car)
library(sp)
library(devtools)
library(qqplotr)
library(MASS)

## Importing Dataset
salary=read.table('/Users/shanshan/Desktop/salary.gender(Project 6-7).txt',
                  sep=",",skip = 25)[,-1]
colnames(salary)=c("Salary","Gender","Education")

str(salary)
sum(is.na(salary)) # No missing data
attach(salary)

#===============================# 
# 1. Exploratory Data Analysis  #
#===============================#

## Frequency Tables
freq.salary<-table(Gender,Education)
mean.freq.salary <-tapply(Salary,list(Gender,Education), mean)

freq.salary #Treatment Frequency
round(mean.freq.salary, digits=2) #Means in the treatments

## Boxplots  

ggplot(salary)+geom_boxplot(aes(x=Gender,y=Salary,fill=Education))

ggplot(salary,aes(x=Education,y=Salary,color=Gender,group=Gender))+
  stat_summary(fun=mean,geom="line")+
  stat_summary(fun=mean,geom="point")+
  stat_summary(fun.data=mean_se,geom="errorbar")

detach(salary)

#===================#
# 2. Anova model    #
#===================#

# Type I SS

Salary.modl<-aov(Salary~Gender*Education,data = salary)
Salary.modl.summary<-summary(Salary.modl)
Salary.modl.summary

## Showing that we used an overparametrized model 
## Factor effects - Regression approach

# Defining the X matrix
salary$Gender<- as.factor(salary$Gender)
contrasts(salary$Gender) = contr.sum
contrasts(salary$Education) = contr.sum
model.matrix(~Gender*Education, salary)

data.for.fit3=as.data.frame(model.matrix(~Gender*Education, salary,
                                         contrasts = list(Gender="contr.sum",Education="contr.sum"))[,2:4])
data.for.fit3$salary=salary$Salary
head(data.for.fit3)


fit.unbalanced=lm(salary~Gender1+Education1+Gender1:Education1,
                  data = data.for.fit3)
summary(fit.unbalanced)
anova(fit.unbalanced)

#======================#
# 3. Model Validation  #
#======================#

#=======================#
# Analysis of residuals #
#=======================#

## Fitted values vs Studentized Residuals
plot(fitted.values(Salary.modl),rstandard(Salary.modl),
     xlab="Fitted values",
     ylab="Studentized residuals",
     main="Residuals vs fitted values plot")
abline(h=0,lty="dashed")

x<-as.numeric(salary$Gender)
plot(x,rstandard(Salary.modl), xlab="Observed values",
     ylab="Studentized residuals",
     main="Residuals vs factor levels plot - Gender",axes=F)

axis(side=1, at = c(1,2), labels = c("Female","Male"))
axis(side=2)
abline(h=0,lty="dashed")

x2<-as.numeric(salary$Education)

plot(x2,rstandard(Salary.modl), xlab="Observed values",
     ylab="Studentized residuals",
     main="Residuals vs factor levels plot - Education",axes=F)

axis(side=1, at = c(1,2), labels = c("Degree","No Degree"))
axis(side=2)
abline(h=0,lty="dashed")

## Sequence Plot for independence 
attach(salary)
n_i=c(length(Salary[Gender=="Male"]),length(Salary[Gender=="Female"]))
n_T=sum(n_i)

plot(rstandard(Salary.modl)[-c(1)],rstandard(Salary.modl)[-c(n_T)],
     xlab="Studentized residuals at 1 lag",
     ylab="Studentized residuals",
     main="Sequence plot")
abline(a=0,b=1,lty="dashed")
detach(salary)

#==================#
#  Normality       # 
#==================#

std.res<-residuals(Salary.modl)
std.res<- stdres(Salary.modl) #Standardized Residuals
std.res<-data.frame(std.res)


#All bands
gg2 <- ggplot(data = std.res, mapping = aes(sample = std.res)) +
  geom_qq_band(bandType = "ks", mapping = aes(fill = "KS"), alpha = 0.5) +
  geom_qq_band(bandType = "ts", mapping = aes(fill = "TS"), alpha = 0.5) +
  geom_qq_band(bandType = "pointwise", mapping = aes(fill = "Normal"), alpha = 0.5) +
  geom_qq_band(bandType = "boot", mapping = aes(fill = "Bootstrap"), alpha = 0.5) +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  scale_fill_discrete("Bandtype")
gg2

## Shapiro - Wilk Test
shapiro.test(Salary.modl$residuals) #Normality is met 
## Kolmogorov Test
ks.test(Salary.modl$residuals,"pnorm",alternative="two.sided")

#=======================================#
# Checking the homogeneity of variances #
#=======================================#

##Checking The model
leveneTest(Salary ~ Gender*Education, salary) # Accepts H0

##Checking Individual Factors
leveneTest(Salary ~ Gender, salary) # Accepts H0
leveneTest(Salary ~ Education, salary) # Accepts H0

#===========================================#
# Testing independece - Durbin Watson test  #
#===========================================#

durbinWatsonTest(Salary.modl, alternative="two.sided", data=salary)

#====================#
# Testing Outliers   #
#====================#

pvalue_outliers = NULL
r.al=2
nT.al<-132
for(i in 1:nT.al)
  pvalue_outliers[i]=1-pt(abs(rstudent(Salary.modl)[i]),
                          + nT.al-r.al-1)

pvalue_outliers[pvalue_outliers>(0.05/(nT.al))]=1
Stud.Deleted.Res=rstudent(Salary.modl)
Outlier.p.value=pvalue_outliers
out.salary<-data.frame(Stud.Deleted.Res,
                       + Outlier.p.value)
out.salary #No Outliers encountered 

which(out.salary > 2)
which(out.salary < -2)

high.res<- out.salary[c(38,47,62,64,85,105,114,121),] 
high.res

## Cook's Distance
plot(cooks.distance(Salary.modl), main = "Cook's Distance")
which(cooks.distance(Salary.modl) > 0.08) #85

#=========================#
# 5. Multiple Comparison  #
#=========================#

##Tukey - Multiple comparison

modl.tukey<-TukeyHSD(Salary.modl,which=c("Gender:Education"), conf.level=.95)
modl.tukey


op0 = par() # Get current graphical parameters
op1 = op0$mar # Get current margins in lines
op1[2] = 15 # Modify bottom margins to 20 (lines)
op1
par(mar = op1)

plot(modl.tukey,col=salary$Gender, las = 1)
par(mar = op0)

#=========================#
# 6.Logistic regression  #
#=========================#

#compare differences between gender
fit1 <- glm(Gender ~ Salary, data = salary, family = "binomial")
summary(fit1)
#compare differences between education
fit2 <- glm(Education ~ Salary, data = salary, family = "binomial")
summary(fit2)
#compare differences between cross-effect
salary1 <- salary[salary$Gender == "Male",]
fit3<-glm(Education ~ Salary, data = salary1, family = "binomial")
summary(fit3)
salary2 <- salary[salary$Gender == "Female",]
fit4<-glm(Education ~ Salary, data = salary2, family = "binomial")
summary(fit4)
salary3 <- salary[salary$Education == "Degree",]
fit5<-glm(Gender ~ Salary, data = salary3, family = "binomial")
summary(fit5)
salary4 <- salary[salary$Education == "No degree",]
fit6<-glm(Gender ~ Salary, data = salary4, family = "binomial")
summary(fit6)
#cross validation
library(boot)
cv1<-cv.glm(salary,fit1)
cv2<-cv.glm(salary,fit2)
cv3<-cv.glm(salary1,fit3)
cv4<-cv.glm(salary2,fit4)
cv5<-cv.glm(salary3,fit5)
cv6<-cv.glm(salary4,fit6)
cv1$delta
cv2$delta
cv3$delta
cv4$delta
cv5$delta
cv6$delta
