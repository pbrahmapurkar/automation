#Install the required packages
#Read the Packages
library(readxl) 
library(psych)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(caret) #to split the data


#Set Working Directory
setwd('D:/Business Analytics/Advanced Analytics and Machine Learning')
#To remove 10E values
options(scipen = 10000)

brain_stroke <- read.csv('stroke.csv')

# :: Summarize the Data ::

summary(brain_stroke)

#Analyze the Columns with NA
colSums(is.na(brain_stroke))

#summary
summary((brain_stroke))

#Change N/A into Mean value
brain_stroke$bmi <- as.numeric((brain_stroke$bmi))
brain_stroke$bmi[is.na(brain_stroke$bmi)] <- mean(brain_stroke$bmi,na.rm = TRUE)

#Convert the categorical Variables into factor
brain_stroke$gender <- as.factor(brain_stroke$gender)
brain_stroke$hypertension <- as.factor(brain_stroke$hypertension)
brain_stroke$heart_disease <- as.factor(brain_stroke$heart_disease)
brain_stroke$ever_married <- as.factor(brain_stroke$ever_married)
brain_stroke$work_type <- as.factor(brain_stroke$work_type)
brain_stroke$Residence_type <- as.factor(brain_stroke$Residence_type)
brain_stroke$smoking_status <- as.factor(brain_stroke$smoking_status)
brain_stroke$stroke <- as.factor(brain_stroke$stroke)

brain_stroke$gender[brain_stroke$gender == 'Other'] <- 'Male'

#::: MEASURES OF ASSOSCIATION::::

#Chi Square Tests in R

#1.cross tabs Function
table(brain_stroke$stroke, brain_stroke$gender)
table(brain_stroke$stroke, brain_stroke$hypertension)
table(brain_stroke$stroke, brain_stroke$heart_disease)
table(brain_stroke$stroke, brain_stroke$ever_married)
table(brain_stroke$stroke, brain_stroke$work_type)
table(brain_stroke$stroke, brain_stroke$Residence_type)
table(brain_stroke$stroke, brain_stroke$smoking_status)

#chisq.test() function to perform the test
chisq.test(brain_stroke$stroke, brain_stroke$gender, correct = FALSE) 
chisq.test(brain_stroke$stroke, brain_stroke$age, correct = FALSE) 
chisq.test(brain_stroke$stroke, brain_stroke$hypertension, correct = FALSE) 
chisq.test(brain_stroke$stroke, brain_stroke$heart_disease, correct = FALSE) 
chisq.test(brain_stroke$stroke, brain_stroke$ever_married, correct = FALSE) 
chisq.test(brain_stroke$stroke, brain_stroke$work_type, correct = FALSE) 
chisq.test(brain_stroke$stroke, brain_stroke$Residence_type, correct = FALSE) 
chisq.test(brain_stroke$stroke, brain_stroke$avg_glucose_level, correct = FALSE) 
chisq.test(brain_stroke$stroke, brain_stroke$bmi, correct = FALSE) 
chisq.test(brain_stroke$stroke, brain_stroke$smoking_status, correct = FALSE) 

#:::: GGPLOT :::::

#::: Count of Strokes Frequency::::
count_stroke <- as.data.frame(table(brain_stroke$stroke))
count_stroke$Var1 <- ifelse(count_stroke$Var1 == 0, "No", 'Yes') 
# Bar Chart of individuals with stroke count
ggplot(count_stroke, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") + theme(legend.position="none") +
  geom_text(aes(label = Freq), vjust = 0) +
  labs(title="Stroke Count of Individuals",x ="Stroke", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))

#Marriage Status
count_marry <- as.data.frame(table(brain_stroke$ever_married))  
ggplot(count_marry, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") + theme(legend.position="none") +
  geom_text(aes(label = Freq), vjust = 0) +
  labs(title="Married Status Bar Chart",x ="Married", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))

# Work Type
count_work <- as.data.frame(table(brain_stroke$work_type))
ggplot(count_work, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") + theme(legend.position="none") +
  geom_text(aes(label = Freq), vjust = 0) +
  labs(title="Work Type Bar chart",x ="Work Type", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))

#Smoking Status
count_smoke <- as.data.frame(table(brain_stroke$smoking_status))
ggplot(count_smoke, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") + theme(legend.position="none") +
  geom_text(aes(label = Freq), vjust = 0) +
  labs(title="Smoking Status",x ="Smoke?", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))

#::::: SPLIT THE LIFEX DATA INTO TRAINING AND TEST:::

#to create a partition with 80%
set.seed(123) #generate a sequence of random numbers
index <- createDataPartition(brain_stroke$stroke, p = 0.8, list = FALSE,)
train <- brain_stroke[index, ] #first 80% for training 
test <- brain_stroke[-index, ] #bottom 20% for testing

#1. :::: Logistic Regression MODEL 1 ::::
formula1 <- stroke ~ gender + hypertension + heart_disease + ever_married + work_type + Residence_type + smoking_status + age + avg_glucose_level + bmi
model1 <- glm(formula1, data = train, family = "binomial")
#Summary of Logistic Regression MODEL 1
summary(model1)
#prediction using the model
predictions1 <- predict(model1,test,type ="response")
#Convert probabilities to 1 or 0
class_pred1 <-as.factor(ifelse(predictions1 > 0.5,1,0))
#evaluate the accuracy of the predictions
postResample(class_pred1,test$stroke)

#Confusion Matrix
confusionMatrix(class_pred1, test$stroke)

#2. :::: Decision Tree MODEL 2 ::::
library(DAAG)
library(party)
library(rpart)
library(rpart.plot)
library(mlbench)
library(caret)
library(pROC)
library(tree)

#Splitting of Data 
set.seed(1234)
ind <- sample(2, nrow(brain_stroke), replace = T, prob = c(0.5, 0.5))
train <- brain_stroke[ind == 1,]
test <- brain_stroke[ind == 2,]

#Creation of Tree
tree <- rpart(stroke ~.-id, data = train)
rpart.plot(tree)
printcp(tree)

#Confusion matrix -train
p <- predict(tree, train, type = 'class')
confusionMatrix(p, train$stroke, positive='1')

p1 <- predict(tree, test, type = 'prob')
p1 <- p1[,2]
r <- multiclass.roc(test$stroke, p1, percent = TRUE)
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid=c(0.1, 0.2),
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve')

#3.::::::: Naives Bayes ::::::

library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

set.seed(1234)
ind <- sample(2, nrow(brain_stroke), replace = T, prob = c(0.8, 0.2))
train <- brain_stroke[ind == 1,]
test <- brain_stroke[ind == 2,]

#Naive Bayes Classification
model <- naive_bayes(stroke ~ .-id, data = train, usekernel = T) 
plot(model) 

p <- predict(model, train, type = 'prob')

#Confusion Matrix - train data
p1 <- predict(model, train)
#Confusion Matrix
(tab1 <- table(p1, train$stroke))
#Accuracy
sum(diag(tab1)) / sum(tab1)

#Confusion Matrix - test data
p2 <- predict(model, test)
#Confusion Matrix
(tab2 <- table(p2, test$stroke))
#Accuracy
(sum(diag(tab2))/sum(tab2))


#4.::::: Random Forest :::::
library(randomForest)
library(datasets)
library(caret)

set.seed(222)
ind <- sample(2, nrow(brain_stroke), replace = TRUE, prob = c(0.7, 0.3))
train <- brain_stroke[ind==1,]
test <- brain_stroke[ind==2,]
rf <- randomForest(stroke~.-id, data=train, proximity=TRUE) 
print(rf)

#Prediction & Confusion Matrix - train data
p1 <- predict(rf, train)
confusionMatrix(p1, train$stroke)

#Prediction & Confusion Matrix - test data
p2 <- predict(rf, test)
confusionMatrix(p2, test_no_id$stroke)

#Error Rate of RF
plot(rf)

#5.::::: Deep Neural Network in R ::::::
library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)

data <- brain_stroke%<>% mutate_if(is.factor, as.numeric)

n <- neuralnet(stroke ~ .-id,
               data = data,
               hidden = c(9,5),
               linear.output = F,
               lifesign = 'full',
               rep=1)

plot(n,col.hidden = 'darkgreen',     
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')


#5. :::::::: XG BOOST::::::::
library(xgboost)
#make this example reproducible
set.seed(0)

# set up the cross-validated hyper-parameter search
XGrid <- expand.grid(
  nrounds = 3500,
  max_depth = 7,
  eta = 0.01,
  gamma = 0.01,
  colsample_bytree = 0.75,
  min_child_weight = 0,
  subsample = 0.5
)

# pack the training control parameters
XControl <- trainControl(
  method = "cv",
  number = 5
)

# train the model for each parameter combination in the grid,
#   using CV to evaluate
xgb_model <- train(
  stroke ~ .-id,
  train,
  method = "xgbTree",
  tuneLength = 3,
  tuneGrid = XGrid,
  trControl = XControl
)
xgb_model
xgb_pred <- predict(xgb_model, newdata = test)
#Confusion Matrix
confusionMatrix(xgb_pred, test$stroke, positive = '1')
