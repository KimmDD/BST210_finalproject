library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)

# load data
diabetes_data <- read.csv("diabetes.csv")

# check the data
head(diabetes_data)
summary(diabetes_data)

# check the missing value
colSums(is.na(diabetes_data))

# no missing value 

# first select insulin as outcome 
summary(diabetes_data$Insulin)

# Some EDA (exploratory data analysis of outcome insulin, and predictors)

# create histogram for outcome
par(mfrow=c(1,2))
hist(diabetes_data$Insulin, main="Histogram of Insulin", xlab="Insulin")
hist(log(diabetes_data$Insulin), main="Histogram of log(Insulin)", xlab="log(Insulin)")

#draw the boxplots of outcome insulin, and predictors
par(mfrow=c(3,2))
boxplot(diabetes_data$Insulin ~ diabetes_data$Age, xlab="age", ylab="insulin(muU/ml")
boxplot(diabetes_data$Insulin ~ diabetes_data$BMI, xlab="BMI", ylab="insulin(muU/ml")
boxplot(diabetes_data$Insulin ~ diabetes_data$Pregnancies, xlab="number of times pregnant", ylab="insulin(muU/ml")
boxplot(diabetes_data$Insulin ~ diabetes_data$Glucose, xlab="glucose", ylab="insulin(muU/ml")
boxplot(diabetes_data$Insulin ~ diabetes_data$BloodPressure, xlab="blood pressure", ylab="insulin(muU/ml")

#draw the histograms of outcome insulin, and predictors
par(mfrow=c(2,3))
hist(diabetes_data$Age, main="Histogram of age", xlab="age")
hist(diabetes_data$BMI, main="Histogram of BMI", xlab="BMI")
hist(diabetes_data$Pregnancies, main="Histogram of number of times pregnant", xlab="number of times pregnant")
hist(diabetes_data$Glucose, main="Histogram of glucose", xlab="glucose")
hist(diabetes_data$BloodPressure, main="Histogram of blood pressure", xlab="blood pressure")


#draw the scatter plots of outcome insulin, and predictors
par(mfrow=c(3,2))
scatter.smooth(as.numeric(diabetes_data$Age), diabetes_data$Insulin, xlab="age", ylab="insulin(muU/ml")
scatter.smooth(diabetes_data$BMI, diabetes_data$Insulin, xlab="BMI", ylab="insulin(muU/ml")
scatter.smooth(diabetes_data$Pregnancies, diabetes_data$Insulin, xlab="number of times pregnant", ylab="insulin(muU/ml")
scatter.smooth(diabetes_data$Glucose, diabetes_data$Insulin, xlab="glucose", ylab="insulin(muU/ml")
scatter.smooth(diabetes_data$BloodPressure, diabetes_data$Insulin, xlab="blood pressure", ylab="insulin(muU/ml")

Iinsulin <- log(diabetes_data$Insulin+1)
pairs(Iinsulin ~ as.numeric(diabetes_data$Age)+diabetes_data$BMI+diabetes_data$Pregnancies+diabetes_data$Glucose+diabetes_data$BloodPressure)
