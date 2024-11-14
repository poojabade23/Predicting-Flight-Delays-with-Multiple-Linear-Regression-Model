#Predicting the Flight Arrival Delays

#Read the data from the working directory, create your own working directly to read the dataset.

setwd("C:/Users/Dell/Desktop/fall 2024/bua 650")

data1 <- read.csv ("Chapter_06_flight_delay.csv",header=TRUE,sep=",")

#Performing Data exploration

#perform exploratory data analysis to know about the data 


# display top 6 rows of dataset to see how data look like

head(data1)


# display bottom 6 rows

tail(data1)

# describe the structure of data

str(data1) 


#display the column name of the data

names(data1)


#display the summary or descriptive statistics of the data

summary(data1$Arr_Delay)


#Let’s check the missing values present in the data 

sum(is.na(data1))

#To find out the correlation between the variables

corr <- cor.test(data1$Arr_Delay,data1$Number_of_flights, method = "pearson" )
# corr

#to add four charts in one window or plotting panel

par(mfrow = c(2,2))

#To plot the dependent and independent variable

plot(data1$Arr_Delay,data1$Number_of_flights)

plot(data1$Arr_Delay,data1$Security_o)

plot(data1$Arr_Delay,data1$Support_Crew_Available)

plot(data1$Arr_Delay,data1$Airport_Distance)  



#To drop the first variable from data1 

data2 <- data1[-c(1)]


#To check the correlation between the variables 
cor(data2)


#Splitting dataset into training and testing dataset

#Install caTools package for splitting the data 

install.packages("caTools")
# 
install.packages("Rtools")
# 
library(caTools)

#To reproduce the sample

set.seed(1000)

sample <- sample.split(data2$Arr_Delay,SplitRatio=0.70)
# sample

#split of the data using subset command

train_data <- subset(data2,sample==TRUE)
test_data <- subset(data2,sample==FALSE)



#Model Building & Interpretation on Training and Testing Data
#Building multiple linear regression model using lm on train_data set

model <- lm(Arr_Delay ~., data = train_data)
# 
summary(model)



#Building final multiple linear regression model with significant variables  on train_data set

# model_sig<-lm(Arr_Delay ~ Airport_Distance+Number_of_flights+Weather+Support_Crew_Available+Baggage_loading_time                                              +Late_Arrival_o, data= train_data)

# model_sig 

#To look at the names of model_sig

# names(model_sig)
#number of the fitted values 

# length(model_sig$fitted.values)


#predicting fitted values of train_data set

# pred_train<- model_sig$fitted.values
# head(pred_train)

# pred_train1 <- data.frame(pred_train)

#residual values

# resed_train <- model_sig$residuals
# head(resed_train)

# resed_train1<-data.frame(resed_train)

#Prediction on the unforeseen dataset, i.e., on  test data

# pred_test<- predict(model_sig,newdata = test_data)
# head(pred_test)


# pred_test1<- data.frame(pred_test)


#Plotting Actual versus Predicted outcome

# plot (test_data$Arr_Delay,col="red",type ="l",lty=1.8 )
# lines(pred_test1,col="blue",type ="l",lty=1.4)


#Linear Regression Diagnostics statistics and plot  

# plot function to view four different diagnostic plots

# To view first plot
# plot(model_sig,which=1)


# Let's look at our second plot now.

# plot(model_sig,which=2)


# Let's look at our third plot now.

# plot(model_sig,which=3)



# Let's look at our final plot(fourth) now.
#Note that this specific plot ID is considered '5' in R.hence   mentioned as which=5.

# plot(model_sig,which=5)



#Install car package
# 
# install.packages("car")
# 
# library(car)


# Test for Independence Assumption 

# durbinWatsonTest(model_sig)


# Statistic Test for Homoscedasticity  Assumption 

# ncvTest(model_sig)


#VIF Test for Collinearity

# vif(model_sig) 


