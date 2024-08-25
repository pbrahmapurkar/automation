#Install the required packages
#Read the Packages
library(readxl) 
library(psych)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(caret) #to split the data
library(Hmisc) #For rcorr() function 
library(corrplot)
library(corrr)
library(data.table)
library(boot)
library(ISLR2)
library(leaps)
library(glmnet)
library(pls)

#Set Workind Directory
setwd('D:/Business Analytics/Advanced Analytics and Machine Learning/Assignment 1')

#Read the excel sheet into variable life
life <- read.csv('Life Expectancy Data.csv')

options(scipen = 100)

#Summarizing the Data
summary(life)

# get means for variables in data frame 
# excluding missing values
sapply((life), mean, na.rm=TRUE)

describe(life)

#::::: Data Quality Issues and Action ::::::

#Caluclate Total NA values
sum(is.na(summary(life)))

#Remove infant.death, BMI, Under five deaths
life <- select(life, -6,-11:-12)

#Resummarize Population below 15000 to median value
life$Population[life$Population < 15000] <- median(life$Population, na.rm = TRUE)

#Re-summarize the Alcohol to Median for NAs
life$Alcohol[is.na(life$Alcohol)] <- median(life$Alcohol, na.rm = TRUE)

#Re-summarize the Polio to Median for NAs
life$Polio[is.na(life$Polio)] <- mean(life$Polio, na.rm = TRUE)

#Re-summarize the Diphtheria  to Mean for NAs
life1$Diphtheria[is.na(life1$Diphtheria)] <- mean(life1$Diphtheria, na.rm = TRUE)

#Re-summarize the thinness..1.19.years  to Median for NAs
life$thinness..1.19.years[is.na(life$thinness..1.19.years)] <- median(life$thinness..1.19.years, na.rm = TRUE)

#Re-summarize the thinness.5.9.years  to Median for NAs
life$thinness.5.9.years[is.na(life$thinness.5.9.years)] <- median(life$thinness.5.9.years, na.rm = TRUE)

#Re-summarize the Schooling to Mean for NAs
life$Schooling[is.na(life$Schooling)] <- median(life$Schooling, na.rm = TRUE)

#Re-summarize the Total Expenditure to Median for NAs
life$Total.expenditure[is.na(life$Total.expenditure)] <- median(life$Total.expenditure, na.rm = TRUE)


#Convert Country into as.factor
life$Country <- as.factor(life$Country)

#Convert Status into as.factor
life$Status <- as.factor(life$Status)

lifex <- life %>% drop_na()
summary(lifex)

#:::::: VISUALIZATION USing GGPLOT:::::::::::

#Life Expectancy Vs Schooling - Geom Points with respect
lifex %>% ggplot(aes(x=lifex$Schooling, y=(lifex$Life.expectancy), colour = lifex$Status))+
  geom_point()+
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 1), colour = 'black')+
  labs(title = 'Comparison of Life Expectancy and Schooling',
       x="Number of years of Schooling(years)", y= "Life Expectancy (AGE)", colour = "Country Status")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


#Life Expectancy for Developed and Developing countries
ggplot(data = lifex, aes(x=Status,y=Life.expectancy, color=Status)) + 
  geom_boxplot()+
  scale_color_brewer(palette="Dark2") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Average Expectancy rate for Developed and Developing countries',
       y='Expectancy rate (Age)',x='Country Status')


#Average Expenditure for Developed and Developing countries
ggplot(lifex)+ 
  geom_histogram(mapping = aes(x = lifex$Status, y=(lifex$percentage.expenditure)),
                 stat = "Summary", fun.y = "mean")+
  labs(title = "Average Expenditure for Developed and Developing countries", x="Country Status", y= 
         "Expenditure on health as a % of GDP per capita")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#HIV AIDS vs Life Expectancy with respect to Status
ggplot (data = lifex, aes (x=Life.expectancy,y=HIV.AIDS, colour = Status)) + geom_point() + geom_smooth(method="loess") +
  ggtitle('Life Expectancy vs HIV AIDS') +
  xlab('Life Expectancy') +
  ylab('Deaths per 1000 live births HIV/AIDS (0-4 years)')

#Life Expectancy vs Alcohol with respect to Status
ggplot (data = lifex, aes (x=Life.expectancy,y=Alcohol, colour = Status)) + geom_point() + geom_smooth(method="loess") +
  ggtitle('Life Expectancy vs Alcohol') +
  xlab('Life Expectancy') +
  ylab('consumption (in litres of pure alcohol)')


#:::: RELATIONSHIPS BETWEEN DIFFERENT VARIABLES ::::::

#CORRPLOT
subdata <- lifex[c("Life.expectancy","Income.composition.of.resources","Schooling","Diphtheria","Polio","Alcohol","percentage.expenditure","Year")]
cor <- cor(subdata)
cor_sort <- as.matrix(sort(cor[,'Life.expectancy'], decreasing = TRUE))
corrplot.mixed(cor, tl.col="black", tl.pos="lt")

#Relationship between Life.expectancy and Income.composition.of.resources with p-value and confidence interval
cor.test(x=lifex$Life.expectancy, y=lifex$Income.composition.of.resources)

#Relationship between Life.expectancy and Total.expenditure with p-value and confidence interval
cor.test(x=lifex$Life.expectancy, y=lifex$Total.expenditure)

#Relationship between Life.expectancy and Polio with p-value and confidence interval
cor.test(x=lifex$Life.expectancy, y=lifex$Polio)

#::::: SPLIT THE LIFEX DATA INTO TRAINING AND TEST:::

#to create a partition with 80%
set.seed(123) #generate a sequence of random numbers
index <- createDataPartition(lifex$Life.expectancy, p = 0.8, list = FALSE,)
train <- lifex[index, ] #first 80% for training 
test <- lifex[-index, ] #bottom 20% for testing



#:::::::::::MULTIPLE LINEAR REGRESSION::::::::::::::

#Create a Multiple Linear regression model
MLR.model <- lm(Life.expectancy ~ (Schooling + Diphtheria + Polio + Alcohol + percentage.expenditure + Year + Adult.Mortality + HIV.AIDS + Income.composition.of.resources + Total.expenditure) , data = train)

#review the model
summary(MLR.model)

#prediction using the model
prediction_1 <- predict(MLR.model, newdata = test)

#(i.e. difference between the actual sale value and the predicted sale value)
postResample(pred = prediction_1, obs = test$Life.expectancy)

#No MultiColinearity
vif(MLR.model)
mean(vif(MLR.model))


#::::::VALIDATION SET APPROACH::::::::::

train <- sample(index, )

## $k$-Fold Cross-Validation
###
set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(Life.expectancy ~ poly(Schooling + Diphtheria + Polio + Alcohol + percentage.expenditure + Year + Adult.Mortality + HIV.AIDS + Income.composition.of.resources + Total.expenditure, i), data = lifex)
  cv.error.10[i] <- cv.glm(lifex, glm.fit, K = 10)$delta[1]
}
cv.error.10


#:::::::::::::::: Subset Selection Method::::::::::::::::

### Best Subset Selection

###
#Check if no row has NA
sum(is.na(lifex$Life.expectancy))
###

regfit.full <- regsubsets(Life.expectancy ~ (Schooling + Diphtheria + Polio + Alcohol + percentage.expenditure + Year + Adult.Mortality + HIV.AIDS + Income.composition.of.resources + Total.expenditure ), lifex)
summary(regfit.full)
###
regfit.full <- regsubsets(Life.expectancy ~ (Schooling + Diphtheria + Polio + Alcohol + percentage.expenditure + Year + Adult.Mortality + HIV.AIDS + Income.composition.of.resources + Total.expenditure),data = lifex,
                          nvmax = 5)
reg.summary <- summary(regfit.full)
###
names(reg.summary)
###
reg.summary$rsq
###
par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables",
     ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")
###
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 2, 
       pch = 20)
###
plot(reg.summary$cp, xlab = "Number of Variables",
     ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = "red", cex = 2,
       pch = 20)
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables",
     ylab = "BIC", type = "l")
points(6, reg.summary$bic[6], col = "red", cex = 2,
       pch = 20)
###
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")
###
coef(regfit.full,5)



###:::::::: Ridge Regression::::::::::::::::::::
### DEFINING x and y 
x <- model.matrix(Life.expectancy ~ Schooling + Diphtheria + Polio + Alcohol + percentage.expenditure + Year + Adult.Mortality + HIV.AIDS + Income.composition.of.resources + Total.expenditure, lifex)
y <- lifex$Life.expectancy

###

grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
###
dim(coef(ridge.mod))
###
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))
###
ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))
###
predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]
###
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]
###
ridge.mod <- glmnet(x[train, ], y[train], alpha = 0,
                    lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ])
mean((ridge.pred - y.test)^2)
###
mean((mean(y[train]) - y.test)^2)
###
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test, ])
mean((ridge.pred - y.test)^2)
###
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ],
                      exact = T, x = x[train, ], y = y[train])
mean((ridge.pred - y.test)^2)
lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, exact = T, type = "coefficients",
        x = x[train, ], y = y[train])[1:20, ]
###
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
###
ridge.pred <- predict(ridge.mod, s = bestlam,
                      newx = x[test, ])

##produce Ridge trace plot
plot(glmnet(x = data.matrix(lifex[, c("Year","Adult.Mortality","Alcohol","percentage.expenditure","Polio","Diphtheria","Schooling","HIV.AIDS", "Income.composition.of.resources", "Total.expenditure")]) , y = lifex$Life.expectancy , alpha = 0), xvar = "lambda")

mean((ridge.pred - y.test)^2)
###RMSE
sqrt(mean((ridge.pred - y.test)^2))
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20, ]

#use fitted best model to make predictions
y_predicted <- predict(ridge.mod, s = bestlam, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
ridge.rsq <- 1 - sse/sst
ridge.rsq

##::::::::::::LASSO REGRESSION::::::::::::

###
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1,
                    lambda = grid)
plot(lasso.mod)
###
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
lasso.pred <- predict(lasso.mod, s = bestlam,
                      newx = x[test, ])
mean((lasso.pred - y.test)^2)
###RMSE
sqrt(mean((lasso.pred - y.test)^2))


out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients",
                      s = bestlam)
lasso.coef
lasso.coef[lasso.coef != 0]

#use fitted best model to make predictions
y_predicted <- predict(lasso.mod, s = bestlam, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
lasso.rsq <- 1 - sse/sst
lasso.rsq

###::::: Principal Components Regression:::::::

###
set.seed(2)
pcr.fit <- pcr(Life.expectancy ~ Schooling + Diphtheria + Polio + Alcohol + percentage.expenditure + Year + Adult.Mortality + HIV.AIDS + Income.composition.of.resources + Total.expenditure, data = lifex, scale = TRUE,
               validation = "CV")
###
summary(pcr.fit)
###
validationplot(pcr.fit, val.type = "MSEP")
###
set.seed(1)
pcr.fit <- pcr(Life.expectancy ~ Schooling + Diphtheria + Polio + Alcohol + percentage.expenditure + Year + Adult.Mortality + + HIV.AIDS + Income.composition.of.resources + Total.expenditure, data = lifex, subset = train,
               scale = TRUE, validation = "CV")
summary(pcr.fit)

#visualize cross-validation plots
validationplot(pcr.fit, val.type = "MSEP")
validationplot(pcr.fit, val.type="R2")
###

train <- lifex[index,]
test <- lifex[-index, ] #bottom 20% for testing
y.test <- lifex[-index,c("Life.expectancy")]
pcr.pred <- predict(pcr.fit, test, ncomp = 5)
mean((pcr.pred - y.test)^2)
###

#calculate RMSE
sqrt(mean((pcr.pred - y.test)^2))

