#setwd("C:/Users/Olive/Desktop/KU Leuven/ANOVA")
AdjunctProf <- adjprof[,1:3]
colnames(AdjunctProf) <- c("payment","subject","degree")
 
## factor A: 1-Humanities, 2-Scial Sciences, 3-Engineering, 4-Management
## factor B:  1-Bachelor, 2-Master, 3-Doctorate
dataset=AdjunctProf

for (i in 1:45){
  if (dataset$subject[i]==1) {
    dataset$subjectName[i] = 'humanities'
  }
  if (dataset$subject[i] == 2) {
    dataset$subjectName[i] = 'social science'
  }
  if (dataset$subject[i] == 3) {
    dataset$subjectName[i] = 'engineering'
  }
  if (dataset$subject[i] == 4) {
    dataset$subjectName[i] = 'management'
  }
}

for (i in 1:45){
  if (dataset$degree[i]==1){
    dataset$degreeName[i]='bachelor'}
  if (dataset$degree[i]==2){
    dataset$degreeName[i]='master'}
  if (dataset$degree[i]==3){
    dataset$degreeName[i]='doctor'}
}
AdjunctProf=dataset[,c(1,4:5)]
colnames(AdjunctProf) <- c("payment","subject","degree")

head(AdjunctProf)
attach(AdjunctProf)
hist(payment,freq = F,breaks=10)
lines(density(payment),col=2)

table(subject,degree)
##It is an unbalanced design

boxplot(payment~subject)
boxplot(payment~degree)
boxplot(payment~degree*subject)


tapply(payment,subject,mean)
tapply(payment,degree,mean)

AdjunctProf$subject <- factor(subject)
AdjunctProf$degree <- factor(degree)


#Interaction plot
interaction.plot(subject, degree, payment, main = "Interaction plot", cex=0.8)

####Cell mean model
lm.AP <- lm(payment~-1+subject:degree,data=AdjunctProf)
summary(lm.AP)


##Test main effect of the subject
subject.unbalanced <- matrix(c(1,-1,0,0,1,-1,0,0,1,-1,0,0,0,1,-1,0,0,1,-1,0,0,1,-1,0,0,0,1,-1,0,0,1,-1,0,0,1,-1),nrow=3)
main.group <- glht(lm.AP, linfct = subject.unbalanced)
summary(main.group,test =Ftest())

degree.unbalanced <- matrix(c(1,1,1,1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,1,1,1,1,-1,-1,-1,-1),nrow=2)
main.sex <- glht(lm.AP, linfct = degree.unbalanced)
summary(main.sex,test =Ftest())


#Test based on the factor effect model
contrasts(AdjunctProf$subject)=contr.sum
contrasts(AdjunctProf$degree)=contr.sum
regre.data.unbalanced=as.data.frame(model.matrix(~ subject*degree, AdjunctProf,contrasts = list(subject="contr.sum",degree="contr.sum"))[,2:12])
regre.data.unbalanced$payment=AdjunctProf$payment
regre.data.unbalanced

full.unbalanced=lm(payment~.,data = regre.data.unbalanced)
summary(full.unbalanced)
anova(full.unbalanced)

NoInt.unbalanced=lm(payment~subject1+subject2+subject3+degree1+degree2, data = regre.data.unbalanced)
summary(NoInt.unbalanced)
anova(NoInt.unbalanced,full.unbalanced)

NoA.unbalanced=lm(payment~.-subject1-subject2-subject3,data = regre.data.unbalanced)
anova(NoA.unbalanced,full.unbalanced)

NoB.unbalanced=lm(payment~.-degree1-degree2,data = regre.data.unbalanced)
anova(NoB.unbalanced,full.unbalanced)

#type I aov:variable in different orders just for comparison with type III below
fit.aov1 <- aov(payment~subject*degree)
summary(fit.aov1)

fit.aov2 <- aov(payment~degree*subject)
summary(fit.aov2)

#type III
fit.type3<-aov(payment~degree*subject,data = AdjunctProf,
               contrasts=list(subject="contr.sum", degree="contr.sum"))
fit.type3
summary(fit.type3)
#only GROUP is Significant

