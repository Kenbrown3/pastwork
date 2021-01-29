######################
### 0. PREPARATION ###
######################
rm(list = ls())
setwd("C:/Users/kebro/OneDrive/KU Leuven/StatCons")
#Importing libraries
library(dplyr)
library(ggplot2)
#install.packages("imputeTS")
library(imputeTS)
library(party)
library(NbClust)
library(factoextra)
library(depmixS4)
library(zoo)
library(randomForest)
library(car)

#Importing data
Machine_data <- read.csv("Machine Data.csv", header = TRUE)
head(Machine_data)
summary(Machine_data)
nrow(Machine_data)

#Transforming the time variable
machine_data_time = Machine_data$Time
class(machine_data_time)

test1 <- gsub(":", ".", machine_data_time)
test2 <- gsub("/", ".", test1)
head(test2)
class(test2)

datestest <- as.POSIXct(strptime(test2, "%e.%m.%Y %H.%M"))
class(datestest)

Machine_data$NewTime = datestest
Machine_data
head(datestest)
class(datestest)
sum(is.na(datestest))

###############################################
### 1. CLEANING AND HANDLING MISSING VALUES ###
###############################################

#Removing duplicates
Machine_data<- unique(Machine_data)[,-2]

#Chronological ordering
Machine_data <- Machine_data[order(Machine_data$NewTime),]

#Lags, to interpolate missing observations
Machine_data$Difference <- c(0,diff(Machine_data$NewTime))
lag_small <- filter(Machine_data, Difference==600)
lag_large <- filter(Machine_data, Difference>600)

#Finding missing times
adding <- lag_small$NewTime-300
for(i in 1:nrow(lag_large)){
  for(j in 1:as.integer((lag_large$Difference[i]-300)/300)){
    adding <- c(adding,lag_large$NewTime[i]-300*j)
  }
}
adding <- rep(adding,3)
Machine_data <- Machine_data[,-4]
new <- data.frame(Measure.Names = rep(c("Control","Output","Input"), each=length(adding)/3),
                  Value = rep(NA, length(adding)), NewTime = adding)
colnames(Machine_data)=colnames(new)
Machine_data <- rbind(Machine_data, new)
Machine_data <- Machine_data[order(Machine_data$NewTime),]

sum(is.na(Machine_data)) #1782 (594 time points)
head(Machine_data)

#Creating seperate datasets for control, input, output
control <- filter(Machine_data, Measure.Names == 'Control')
control
input <- filter(Machine_data, Measure.Names == 'Input')
input
output <- filter(Machine_data, Measure.Names == 'Output')
output

#Interpolation for NA values
control$Value <- na_interpolation(control$Value)
input$Value <- na_interpolation(input$Value)
output$Value <- na_interpolation(output$Value)

#Forming the clean dataset
Machine_data <- rbind(control, input, output)
Machine_data <- Machine_data[order(Machine_data$NewTime),]
sum(is.na(Machine_data)) #0

###################
### 2. PLOTTING ###
###################

#Plotting the whole data
#Quite messy.

ggplot(Machine_data,aes(x=NewTime,y=Value))+
  geom_point()

#Grouping by Measure.Names
scatterplot_fulldata <- ggplot(Machine_data[Machine_data$Measure.Names!='Control',], aes(NewTime, Value, colour=factor(Measure.Names)))
scatterplot_fulldata + geom_line() #Better visualization.

#Line plots and histograms for each Measure.Names level
scatterplot_control <- ggplot(control, aes(NewTime, Value))
scatterplot_control + geom_line(colour="red") + labs(title = "Control")
histogram_control <- ggplot(control, aes(Value))
histogram_control + geom_histogram(bins=40,colour="black", fill="green")

scatterplot_input <- ggplot(input, aes(NewTime, Value))
scatterplot_input + geom_line(colour="green") + labs(title = "Input")
histogram_input <- ggplot(input, aes(Value))
histogram_input + geom_histogram(bins=50,colour="black", fill="green")

scatterplot_output <- ggplot(output, aes(NewTime, Value))
scatterplot_output + geom_line(colour="blue") + labs(title = "Output")
histogram_output <- ggplot(output, aes(Value))
histogram_output + geom_histogram(bins=50,colour="black", fill="green")

##############################
### 3. HIDDEN MARKOV MODEL ###
##############################
#create dataset with intput and output values together
head(input)
head(output)
input_values <- input$Value
output_values <- output$Value
dataset_HMM <- cbind(input_values, output_values)
dataset_HMM <- as.data.frame(dataset_HMM)
head(dataset_HMM)

# Determine number of clusters
#pkgs <- c("factoextra",  "NbClust")
#install.packages(pkgs)


df <- scale(dataset_HMM)
head(df)

#create subset of the data to identify clusters
set.seed(123)
smp_siz <- floor(0.20*nrow(df))
train_ind <- sample(seq_len(nrow(df)),size = smp_siz)
train <- df[train_ind,]
test <- df[-train_ind,]
nrow(train)

# Elbow method for clustering
fviz_nbclust(train, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = "Elbow method")
#According to plot, 3 clusters is the optimum.

#Fit Hidden Markov Model with 3 states
#install.packages("depmixS4")

mod <- depmix(output_values ~ input_values, family = gaussian(), nstates = 3, data = dataset_HMM)
fit <- fit(mod)
summary(fit)

#Try to estimate the state for every observation
probs <- posterior(fit)
head(probs)
head(dataset_HMM)
dataset_HMM_3states <- dataset_HMM
dataset_HMM_3states$State <- probs$state
dataset_HMM_3states$Time <- input$NewTime
head(dataset_HMM_3states)
#Plot the state for every observation vs Time
g1 <- ggplot(dataset_HMM_3states, aes(x = Time, y = State, fill = State, col = State)) +
  geom_bar(stat = "identity", alpha = I(0.7))
g1 + ylim(0, 5)

#Fit Hidden Markov Model with 5 states
mod5 <- depmix(output_values ~ input_values, family = gaussian(), nstates = 5, data = dataset_HMM)
fit5 <- fit(mod5)
summary(fit5)

#Try to estimate the state for every observation
probs5 <- posterior(fit5)
head(probs5)
head(dataset_HMM)
dataset_HMM_5states <- dataset_HMM
dataset_HMM_5states$State <- probs5$state
dataset_HMM_5states$Time <- input$NewTime
head(dataset_HMM_5states)
#Plot the state for every observation vs Time
g2 <- ggplot(dataset_HMM_5states, aes(x = Time, y = State, fill = State, col = State)) +
  geom_bar(stat = "identity", alpha = I(0.7))
g2 + ylim(0, 5)

#Fit Hidden Markov Model with 7 states
mod7 <- depmix(output_values ~ input_values, family = gaussian(), nstates = 7, data = dataset_HMM)
fit7 <- fit(mod7)
summary(fit7)

#Try to estimate the state for every observation
probs7 <- posterior(fit7)
head(probs7)
head(dataset_HMM)
dataset_HMM_7states <- dataset_HMM
dataset_HMM_7states$State <- probs7$state
dataset_HMM_7states$Time <- input$NewTime
head(dataset_HMM_7states)
#Plot the state for every observation vs Time
g3 <- ggplot(dataset_HMM_7states, aes(x = Time, y = State, fill = State, col = State)) +
  geom_bar(stat = "identity", alpha = I(0.7))
g3 + ylim(0, 7)

##########################
### 4. FURTHER STUDIES ###
##########################
#SMOOTHING and PREPOCESSING
#This determines the input data change to dec. 10 data when needed
datain <- dataset_HMM

#which smoothing intervals should be tested
lowerbound <- 50
upperbound <- 5000
interval <- 50
sequence <- seq(lowerbound,upperbound,by=interval)

#number of hidden states
nclust <- 5

#result  matrix, view to examine which smoothing intervals are the most unique
matrixdiag <- matrix(rep(rep(0,length(sequence)),nclust),nrow=nclust)
colnames(matrixdiag) <- sequence

#matrix for AIC values
aicmatrix <- matrix(rep(0,length(sequence)),nrow=1)
colnames(aicmatrix) <- sequence

#filling of the result matrix, change dependencies above to get a new matrix
colind <- 1


for(i in sequence){
  dataset_HMM2 <- datain
  dataset_HMM2$smoothdata_in <- rollmean(dataset_HMM2$input_values,i,fill=NA,align="right")
  dataset_HMM2$smoothdata_out <- rollmean(dataset_HMM2$output_values,i,fill=NA,align="right")
  modsmooth <- depmix(smoothdata_out ~ smoothdata_in, family = gaussian(), nstates = nclust,data=dataset_HMM2)
  fitsmooth <- fit(modsmooth)
  matrixdiag[1:nclust,colind] <- matrix(getpars(fitsmooth)[seq(nclust+1,(nclust+1)*nclust,by=nclust+1)],nrow=nclust)
  aicmatrix[colind] <- AIC(fitsmooth)
  colind <- colind+1
}

#CONSIDERING THE DATA AFTER DECEMBER 1ST, 2009
head(Machine_data)
after_december <- filter(Machine_data, NewTime > "2019-12-01 00:00:00")

controldec1 <- filter(after_december, Measure.Names == 'Control')
controldec1
inputdec1 <- filter(after_december, Measure.Names == 'Input')
inputdec1
outputdec1 <- filter(after_december, Measure.Names == 'Output')
outputdec1

input_valuesdec1 <- inputdec1$Value
output_valuesdec1 <- outputdec1$Value
dataset_HMMdec1 <- cbind(input_valuesdec1, output_valuesdec1)
dataset_HMMdec1 <- as.data.frame(dataset_HMMdec1)
head(dataset_HMMdec1)

dfdec <- scale(dataset_HMMdec1)
head(dfdec)

smp_sizdec <- floor(0.20*nrow(dfdec))
train_inddec <- sample(seq_len(nrow(dfdec)),size = smp_sizdec)
traindec <- dfdec[train_inddec,]
testdec <- dfdec[-train_inddec,]
nrow(traindec)

fviz_nbclust(traindec, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")
#Again, 3 clusters is the optimum.

moddec <- depmix(output_valuesdec1 ~ input_valuesdec1, family = gaussian(), nstates = 3, data = dataset_HMMdec1)
fitdec <- fit(moddec)
summary(fitdec)

#Try to estimate the state for every observation
probsdec <- posterior(fitdec)
head(probsdec)
dataset_HMM_3states_dec <- dataset_HMMdec1
dataset_HMM_3states_dec$State <- probsdec$state
dataset_HMM_3states_dec$Time <- inputdec1$NewTime
head(dataset_HMM_3states_dec)
#Plot for dec1 data
gdec1 <- ggplot(dataset_HMM_3states_dec, aes(x = Time, y = State, fill = State, col = State)) +
  geom_bar(stat = "identity", alpha = I(0.7))
gdec1 + ylim(0, 5)

#######################################
### 5. PREDICTION MODEL FOR CONTROL ###
#######################################
#install.packages("randomForest")

head(control)
head(dataset_HMM_3states)
dataset_HMM_3states$control_values <- control$Value

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(100)
train <- sample(nrow(dataset_HMM_3states), 0.7*nrow(dataset_HMM_3states), replace = FALSE)
TrainSet <- dataset_HMM_3states[train,]
ValidSet <- dataset_HMM_3states[-train,]
summary(TrainSet)
head(TrainSet)
summary(ValidSet)
head(ValidSet)

#FINDING A SUITABLE NUMBER OF TREE FOR RANDOM FOREST
rf_model_50 <- randomForest(control_values~input_values+output_values+State, data=TrainSet, importance=T, ntree=50)
rf_model_100 <- randomForest(control_values~input_values+output_values+State, data=TrainSet, importance=T, ntree=100)
rf_model_150 <- randomForest(control_values~input_values+output_values+State, data=TrainSet, importance=T, ntree=150)
rf_model_200 <- randomForest(control_values~input_values+output_values+State, data=TrainSet, importance=T, ntree=200)
rf_model_250 <- randomForest(control_values~input_values+output_values+State, data=TrainSet, importance=T, ntree=250)
rf_model_300 <- randomForest(control_values~input_values+output_values+State, data=TrainSet, importance=T, ntree=300)
rf_model_350 <- randomForest(control_values~input_values+output_values+State, data=TrainSet, importance=T, ntree=350)
rf_model_400 <- randomForest(control_values~input_values+output_values+State, data=TrainSet, importance=T, ntree=400)
rf_model_450 <- randomForest(control_values~input_values+output_values+State, data=TrainSet, importance=T, ntree=450)
rf_model_500 <- randomForest(control_values~input_values+output_values+State, data=TrainSet, importance=T, ntree=500)

predValid_50 <- predict(rf_model_50, ValidSet, type = "class")
predValid_100 <- predict(rf_model_100, ValidSet, type = "class")
predValid_150 <- predict(rf_model_150, ValidSet, type = "class")
predValid_200 <- predict(rf_model_200, ValidSet, type = "class")
predValid_250 <- predict(rf_model_250, ValidSet, type = "class")
predValid_300 <- predict(rf_model_300, ValidSet, type = "class")
predValid_350 <- predict(rf_model_350, ValidSet, type = "class")
predValid_400 <- predict(rf_model_400, ValidSet, type = "class")
predValid_450 <- predict(rf_model_450, ValidSet, type = "class")
predValid_500 <- predict(rf_model_500, ValidSet, type = "class")

rmse_50 <- mean((predValid_50 - ValidSet$control_values)**2)**0.5
rmse_100 <- mean((predValid_100 - ValidSet$control_values)**2)**0.5
rmse_150 <- mean((predValid_150 - ValidSet$control_values)**2)**0.5
rmse_200 <- mean((predValid_200 - ValidSet$control_values)**2)**0.5
rmse_250 <- mean((predValid_250 - ValidSet$control_values)**2)**0.5
rmse_300 <- mean((predValid_300 - ValidSet$control_values)**2)**0.5
rmse_350 <- mean((predValid_350 - ValidSet$control_values)**2)**0.5
rmse_400 <- mean((predValid_400 - ValidSet$control_values)**2)**0.5
rmse_450 <- mean((predValid_450 - ValidSet$control_values)**2)**0.5
rmse_500 <- mean((predValid_500 - ValidSet$control_values)**2)**0.5

rmse <- c(rmse_50, rmse_100, rmse_150, rmse_200, rmse_250, rmse_300, rmse_350, rmse_400, rmse_450, rmse_500)
plot(rmse)
#BEST NUMBER OF TREE: 100

# Create a Random Forest model with best found number of tree
rf_model <- randomForest(control_values ~ input_values + output_values + State, data = TrainSet, importance = TRUE, ntree=100)
rf_model
importance(rf_model)
?importance
varImpPlot(rf_model)

importance = importance(rf_model)
varImportance = data.frame(Variables = row.names(importance),
                           Importance =round(importance[, "IncNodePurity"],2))

rankImportance=varImportance%>%mutate(Rank=paste("#",dense_rank(desc(Importance))))

ggplot(rankImportance,aes(x=reorder(Variables,Importance), y=Importance,fill=Importance)) + geom_bar(stat = "Identity") + geom_text(aes(x = Variables, y = 0.5, label = Rank),
                                                                                                                                    hjust=0, vjust=0.55, size = 4, colour = "white") +
  labs(x = "Variables") + coord_flip() + theme_classic()

varImportance = data.frame(Variables = row.names(importance),
                           Importance =round(importance[, "%IncMSE"],2))

rankImportance=varImportance%>%mutate(Rank=paste("#",dense_rank(desc(Importance))))

ggplot(rankImportance,aes(x=reorder(Variables,Importance), y=Importance,fill=Importance)) + geom_bar(stat = "Identity") + geom_text(aes(x = Variables, y = 0.5, label = Rank),
                                                                                                                                    hjust=0, vjust=0.55, size = 4, colour = "white") +
  labs(x = "Variables") + coord_flip() + theme_classic()

#Another random forrest method. This method is more robust when multifactorial variables are involved
#Traditional Random Forests are more biased towards factor variables with many levels/categories

cf1 <- cforest(control_values ~ input_values + output_values + State, data = TrainSet,control=cforest_unbiased(mtry=2,ntree=100))
varimp(cf1)
varimp(cf1,conditional=TRUE)

#Predicting on Validation set
predValid <- predict(rf_model, ValidSet, type = "class")
head(predValid)
head(ValidSet$control_values)



######################################################
### 6. COMPARISON BETWEEN PREDICTION MODEL AND HMM ###
######################################################



#Method 1: Linear regression and checking variable effect
linear_model <- lm(control_values ~ input_values + output_values + State, data=dataset_HMM_3states)
summary(linear_model) #State effect is not significant

#Method 2: Anova test for models with and without state
model1 <- lm(control_values ~ input_values + output_values + State, data=dataset_HMM_3states)
model2 <- lm(control_values ~ input_values + output_values, data=dataset_HMM_3states)
Anova(model1, model2)
anova(model1, model2) #State effect is not significant

#Linear model is wrong, because variables aren't normally distributed.

#Method 3: Paried t-test for errors in validation set
m1 <- randomForest(control_values ~ input_values + output_values + State, data = TrainSet, importance = TRUE, ntree=100)
m2 <- randomForest(control_values ~ input_values + output_values, data = TrainSet, importance = TRUE, ntree=100) #Too long computation
pred1 <- predict(m1, ValidSet, type="class")
pred2 <- predict(m2, ValidSet, type="class")
#Error type: Regular
error1 <- (pred1 - ValidSet$control_values)
error2 <- (pred2 - ValidSet$control_values)
t.test(error1, error2, paired=T, alternative="two.sided") #Not significant
#Error type: Absolute
error1 <- abs((pred1 - ValidSet$control_values))
error2 <- abs((pred2 - ValidSet$control_values))
t.test(error1, error2, paired=T, alternative="two.sided") #Significant
#Error type: Squared
error1 <- (pred1 - ValidSet$control_values)**2
error2 <- (pred2 - ValidSet$control_values)**2
t.test(error1, error2, paired=T, alternative="two.sided") #Significant
#Error type: Square-rooted
error1 <- (pred1 - ValidSet$control_values)**0.5
error2 <- (pred2 - ValidSet$control_values)**0.5
t.test(error1, error2, paired=T, alternative="two.sided") #Significant


smoothdata_in=rollmean(dataset_HMM$input_values,2000,align="right",fill = NA)
plot(smoothdata_in)





