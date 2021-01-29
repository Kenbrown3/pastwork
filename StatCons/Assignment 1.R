rm(list=ls())
library(dplyr)
library(ggplot2)
#install.packages("imputeTS")
library(imputeTS)
#pkgs <- c("factoextra",  "NbClust")
#install.packages(pkgs)
library(factoextra)
library(NbClust)
library(depmixS4)

#Import data
Machine_data <- read.csv(choose.files(), header = TRUE)
Machine_data
summary(Machine_data)
nrow(Machine_data)

#fixing variable name
Machine_data=Machine_data%>%
  rename(Measure.Names=ï..Measure.Names)

#Transform time variable
machine_data_time = Machine_data$Time
class(machine_data_time)

test1 <- gsub(":", ".", machine_data_time) 
test2 <- gsub("/", ".", test1) 
test2
class(test2)

datestest <- as.POSIXct(strptime(test2, "%e.%m.%Y %H.%M"))
class(datestest)

Machine_data$NewTime = datestest
Machine_data
head(datestest)
class(datestest)
sum(is.na(datestest))


#####################
### DATA CLEANING ###
#####################

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
Machine_data <- rbind(Machine_data, new)
Machine_data <- Machine_data[order(Machine_data$NewTime),]

########################################


#create seperate datasets for control, input, output
control <- filter(Machine_data, Measure.Names == 'Control')
control
input <- filter(Machine_data, Measure.Names == 'Input')
input
output <- filter(Machine_data, Measure.Names == 'Output')
output

#####################
### INTERPOLATION ###
#####################

control$Value <- na_interpolation(control$Value)
input$Value <- na_interpolation(input$Value)
output$Value <- na_interpolation(output$Value)
####################################################################################################################################


#Plot the data
plot(Machine_data$NewTime, Machine_data$Value)

scatterplot_fulldata <- ggplot(Machine_data, aes(NewTime, Value, colour=factor(Measure.Names)))
scatterplot_fulldata
scatterplot_fulldata + geom_point()
scatterplot_fulldata + geom_line()

scatterplot_control <- ggplot(control, aes(NewTime, Value))
scatterplot_control
scatterplot_control + geom_point()
scatterplot_control + geom_line(colour="black") + labs(title = "Control")
histogram_control <- ggplot(control, aes(Value)) 
histogram_control
histogram_control + geom_histogram(bins=40,colour="black", fill="green")

scatterplot_input <- ggplot(input, aes(NewTime, Value))
scatterplot_input
scatterplot_input + geom_point()
scatterplot_input + geom_line(colour="black") + labs(title = "Input")
histogram_input <- ggplot(input, aes(Value)) 
histogram_input
histogram_input + geom_histogram(bins=50,colour="black", fill="green")

scatterplot_output <- ggplot(output, aes(NewTime, Value))
scatterplot_output
scatterplot_output + geom_point()
scatterplot_output + geom_line(colour="black") + labs(title = "Output")
histogram_output <- ggplot(output, aes(Value)) 
histogram_output
histogram_output + geom_histogram(bins=50,colour="black", fill="green")

#create dataset with intput and output values together
input_values <- input$Value
output_values <- output$Value
dataset_HMM <- cbind(input_values, output_values)
dataset_HMM <- as.data.frame(dataset_HMM)
head(dataset_HMM)

# Determine number of clusters

df <- scale(dataset_HMM)
head(df)

#create subset of the data to identify clusters 
set.seed(123) 
smp_siz = floor(0.20*nrow(df))
train_ind = sample(seq_len(nrow(df)),size = smp_siz)  
train =df[train_ind,] 
test=df[-train_ind,] 
nrow(train)

# Elbow method for clustering
fviz_nbclust(train, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")

#Fit Hidden Markov Model
mod <- depmix(output_values ~ input_values, family = gaussian(), nstates = 3, data = dataset_HMM)
fit <- fit(mod)
summary(fit)



