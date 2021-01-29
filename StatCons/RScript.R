#Import data
Machine_data = read.csv(choose.files(), header = TRUE)
Machine_data
summary(Machine_data)
nrow(Machine_data)

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

#create seperate datasets for control, input, output
library(tidyverse)
control <- filter(Machine_data, ï..Measure.Names == 'Control')
control
input <- filter(Machine_data, ï..Measure.Names == 'Input')
input
output <- filter(Machine_data, ï..Measure.Names == 'Output')
output

#Plot the data
plot(Machine_data$NewTime, Machine_data$Value)

scatterplot_fulldata <- ggplot(Machine_data, aes(NewTime, Value, colour=factor(ï..Measure.Names)))
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







