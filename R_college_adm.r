install.packages("readr")
library(readr)
data <- read_csv("C:/Users/User/Desktop/simplilearn/Assignment/R_programming/College_admission.csv")
data

#Find the missing values.
sum(is.na(data))

#Find outliers 
outdata<-data # Copy old dataset
boxplot(outdata$gre, xlab='gre')
boxplot(outdata$gpa, xlab='gpa')
boxplot(outdata$gre)$out
boxplot(outdata$gpa)$out
outdata<-outdata[-which(outdata$gre %in% boxplot(outdata$gre)$out),]# remove the gre outliers
outdata<-outdata[-which(outdata$gpa %in% boxplot(outdata$gpa)$out),]# remove the gpa outliers

#Find the structure of the data set and if required, transform the numeric data type to factor and vice-versa.
str(outdata)
convertdata<-outdata
convertdata$admit<-as.factor(convertdata$admit)
convertdata$ses<-as.factor(convertdata$ses)
convertdata$Gender_Male<-as.factor(convertdata$Gender_Male)
convertdata$Race<-as.factor(convertdata$Race)
convertdata$rank<-as.factor(convertdata$rank)
str(convertdata)

#Find whether the data is normally distributed or not. Use the plot to determine the same.
#create histogram for both datasets
hist(data$gre, col='steelblue', main='gre')
hist(data$gpa, col='steelblue', main='gpa')
#create Q-Q plot for both datasets
qqnorm(data$gre, main='gre', col='steelblue')
qqline(data$gre, col='steelblue')

qqnorm(data$gpa, main='gpa', col='steelblue')
qqline(data$gpa, col='steelblue')
#perform shapiro-wilk test
shapiro.test(data$gre)
shapiro.test(data$gpa)
#less than 0.05,which indicates that the data is not normally distributed

#Normalize the data if not normally distributed.
#define Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
#apply Min-Max normalization to columns in dataset
data_norm <- convertdata
data_norm$gre <- min_max_norm(data_norm$gre)
hist(data_norm$gre, col='steelblue', main='gre')
data_norm$gpa <- min_max_norm(data_norm$gpa)
hist(data_norm$gpa, col='steelblue', main='gpa')

#Use variable reduction techniques to identify significant variables.
data_norm
logistic_model_all <- glm(admit ~ gre + gpa + ses + Gender_Male + Race + rank, 
                      data = data_norm, 
                      family = "binomial")
summary(logistic_model_all)
#gre, gpa, rank p-value are less than 0.05 that they have a statistically significant relationship with the response variable in the model.

#Run logistic model to determine the factors that influence the admission process of a student (Drop insignificant variables) 
logistic_model_vr <- glm(admit ~ gre + gpa + rank, 
                          data = data_norm, 
                          family = "binomial")
summary(logistic_model_vr)

#Calculate the accuracy of the model and run validation techniques.
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(data_norm), replace=TRUE, prob=c(0.7,0.3))
train  <- data_norm[sample, ]
test   <- data_norm[!sample, ]
logistic_model <- glm(admit ~ gre + gpa + rank, 
                      data = train, 
                      family = "binomial"(link="logit"))
summary(logistic_model)
log_predict <-predict(logistic_model, test,  type="response")
log_predict
install.packages('caret')
library(caret)
str(test$admit)
str(log_predict)
log_predict<- ifelse(log_predict > 0.5, "1", "0")
log_predict<- as.factor(log_predict)
confusionMatrix(test$admit, log_predict)

#Try other modelling techniques like decision tree and SVM and select a champion model 
#decision tree
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
decisiontree_model<- rpart(admit ~ gre + gpa + rank, data=train, method = 'class')
rpart.plot(decisiontree_model, extra = 106)
dt_predict <-predict(decisiontree_model, test, type = 'class')
confusionMatrix(test$admit, dt_predict)
#SVM
install.packages('e1071')
library(e1071)
SVM_model <- svm(admit ~ gre + gpa + rank, data = train, type = 'C-classification', kernel = 'linear')
SVM_model
svm_predict <-predict(SVM_model, test)
confusionMatrix(test$admit, svm_predict)
#from accuracy,logistic_model is a champion model

#Categorize the average of grade point into High, Medium, and Low 
cadata<-outdata
cadata$categorygre <- cut(outdata$gre, breaks=c(0, 400, 580, Inf), labels=c("low","middle","high"))
cadata
library(purrr)
library(tidyr)
library(ggplot2)
ggplot(gather(cadata, key, value, -c(categorygre)), aes(value, fill=factor(categorygre))) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')


