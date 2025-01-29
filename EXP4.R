library(Mlbench) 
library(dplyr) 
library(ggplot2) 
install.packages("car", dependencies = TRUE) 
data<-read.csv("C://Users//ramar//Downloads//Telegram Desktop//diabetes.csv") 
str(data) 
summary(data) 
cor_matrix <- cor(data[, -9]) 
print(cor_matrix) 
pairs(data) 
library(car)
library(carData)
scatterplotMatrix(data, method = "loess") 
ggplot(data, aes(x = Glucose)) + 
  geom_histogram(binwidth = 10, fill = "blue", color = "black") + 
  labs(title = "Distribution of Glucose Levels") 
ggplot(data, aes(x = factor(DiabetesPedigreeFunction), y = Glucose)) + 
  geom_boxplot(fill = "green") + 
  labs(title = "Glucose Levels by Diabetes Status", x = "Diabetes Status", y = "Glucose Levels")
