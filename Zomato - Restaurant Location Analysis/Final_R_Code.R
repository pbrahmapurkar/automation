#::::: LOAD THE DIRECTORY ::::::
setwd('D:/Dissertation')

#::::: LOAD ESSENTIAL LIBRARIES ::::::
library(writexl)
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(caret)
library(logistf)
library(glmnet)
library(plotly)

#:::: READ THE ZOMATO FILE CONTAINING ALL THE ATTRIBUTES ::::::
#Read the CSV file and replace empty cells by NA
zomato <- read.csv('zomato.csv',na.strings = c("", "NA"))
#Check NA values
sum(is.na(zomato))
colSums(is.na(zomato))

#check the blank values
sapply(zomato,function(x) sum(x==""))

#Drop Unnecesary Columns
zomato <- subset(zomato, select = -c(url,phone,reviews_list,menu_item) )

#Remove Duplicate Rows
zomato <- distinct(zomato)

#Remove '/5' extra string from the rating column. so, that 
zomato$rate <- gsub("/5","",as.character(zomato$rate))

#Remove NAs from Zomato
zomato <- drop_na(zomato)

#Check if Duplicates are Present
sum(duplicated(zomato))

#Check if NA values from the dataset
sum(is.na(zomato))

#Remove NAs from Zomato
zomato <- drop_na(zomato)

#Check if Duplicates are Present
sum(duplicated(zomato))

#Check if NA values from the dataset
sum(is.na(zomato))

#Write the updated Excel File for combining 2 files in MS-ACCESS USING SQL QUERIES
write_xlsx(zomato,"UPDATED_ZOMATO.xlsx")

#:::::::AFTER COMBINING THE 2 TABLES in MS-ACCESS WITH NEW VARIABLES Distance from City Centre and High Activity PLACE ::::::: 
#Read the FINAL CSV file and replace empty cells by NA
zomato <- read.csv('zom.csv',na.strings = c("", "NA"))

#Drop Unnecesary Columns
zomato <- subset(zomato, select = -c(dish_liked, cuisines, listed_in.type.  ) )


#::::::: DATA CLEANING::::::::::::

#Remove '/5' extra string from the rating column. so, that 
zomato$rate <- gsub("/5","",as.character(zomato$rate))

#Create a New Variable Satisfaction_Level - Positive or Negative
#Previously it was tested with rate >=3 but rate>=4 are giving better results
zomato <- zomato %>%
  mutate(Satisfaction_Rate = case_when(
    rate >= 4 ~ "Positive",
    rate < 4 ~ "Negative"
  ))

#Convert the Required Columns into Factors
zomato$name <- as.factor(zomato$name)
zomato$online_order <- as.factor(zomato$online_order)
zomato$book_table <- as.factor(zomato$book_table)
zomato$location <- as.factor(zomato$location)
zomato$rest_type <- as.factor(zomato$rest_type)
zomato$listed_in.city. <- as.factor(zomato$listed_in.city.)
zomato$location2 <- as.factor(zomato$location2)
zomato$Satisfaction_Rate  <- as.factor(zomato$Satisfaction_Rate)
zomato$happening_place <- as.factor(zomato$happening_place)

#Summary of New created Variable
summary(zomato$Satisfaction_Rate)

#Convert into number
zomato$rate <- as.numeric(zomato$rate)
zomato$approx_cost.for.two.people. <- as.integer(zomato$approx_cost.for.two.people.)

#Check and Reorder Levels
levels(zomato$Satisfaction_Rate)

#Rename the levels for Restaurant Type
zomato$rest_type[zomato$rest_type == 'Bakery, Cafe'] <- 'Bakery'
zomato$rest_type[zomato$rest_type == 'Bakery, Dessert Parlor'] <- 'Bakery'
zomato$rest_type[zomato$rest_type == 'Bakery, Quick Bites'] <- 'Bakery'
zomato$rest_type[zomato$rest_type == 'Bar, Casual Dining'] <- 'Bar'
zomato$rest_type[zomato$rest_type == 'Bar, Pub'] <- 'Bar'
zomato$rest_type[zomato$rest_type == 'Pub'] <- 'Bar'
zomato$rest_type[zomato$rest_type == 'Beverage Shop, Cafe'] <- 'Beverage Shop'
zomato$rest_type[zomato$rest_type == 'Beverage Shop, Dessert Parlor'] <- 'Beverage Shop'
zomato$rest_type[zomato$rest_type == 'Beverage Shop, Quick Bites'] <- 'Beverage Shop'
zomato$rest_type[zomato$rest_type == 'Cafe, Bakery'] <- 'Cafe'
zomato$rest_type[zomato$rest_type == 'Cafe, Dessert Parlor'] <- 'Cafe'
zomato$rest_type[zomato$rest_type == 'Cafe, Quick Bites'] <- 'Cafe'
zomato$rest_type[zomato$rest_type == 'Cafe, Casual Dining'] <- 'Casual Dining'
zomato$rest_type[zomato$rest_type == 'Casual Dining, Bar'] <- 'Casual Dining'
zomato$rest_type[zomato$rest_type == 'Casual Dining, Pub'] <- 'Casual Dining'
zomato$rest_type[zomato$rest_type == 'Casual Dining, Cafe'] <- 'Casual Dining'
zomato$rest_type[zomato$rest_type == 'Casual Dining, Lounge'] <- 'Casual Dining'
zomato$rest_type[zomato$rest_type == 'Food Court, Casual Dining'] <- 'Casual Dining'
zomato$rest_type[zomato$rest_type == 'Dessert Parlor, Bakery'] <- 'Dessert Parlor'
zomato$rest_type[zomato$rest_type == 'Dessert Parlor, Beverage Shop'] <- 'Dessert Parlor'
zomato$rest_type[zomato$rest_type == 'Dessert Parlor, Cafe'] <- 'Dessert Parlor'
zomato$rest_type[zomato$rest_type == 'Dessert Parlor, Kiosk'] <- 'Dessert Parlor'
zomato$rest_type[zomato$rest_type == 'Dessert Parlor, Quick Bites'] <- 'Dessert Parlor'
zomato$rest_type[zomato$rest_type == 'Food Court, Quick Bites'] <- 'Food Court'
zomato$rest_type[zomato$rest_type == 'Quick Bites, Cafe'] <- 'Quick Bites'
zomato$rest_type[zomato$rest_type == 'Quick Bites, Dessert Parlor'] <- 'Quick Bites'
zomato$rest_type[zomato$rest_type == 'Quick Bites, Food Court'] <- 'Quick Bites'
zomato$rest_type[zomato$rest_type == 'Quick Bites, Bakery'] <- 'Quick Bites'
zomato$rest_type[zomato$rest_type == 'Quick Bites, Beverage Shop'] <- 'Quick Bites'
zomato$rest_type[zomato$rest_type == 'Sweet Shop, Quick Bites'] <- 'Sweet Shop'
zomato$rest_type[zomato$rest_type == 'Quick Bites, Sweet Shop'] <- 'Sweet Shop'
zomato$rest_type[zomato$rest_type == 'Takeaway'] <- 'Takeaway'
zomato$rest_type[zomato$rest_type == 'Takeaway, Delivery'] <- 'Takeaway'

#Remove Restaurant Type with Dhaba, Food Truck, Mess and Kiosk as there are very few restaurants for the same
zomato <- zomato[!(zomato$rest_type == "Food Truck" | zomato$rest_type == "Kiosk" | zomato$rest_type == "Mess" | zomato$rest_type == "Dhaba"),]

#drop unused factor levels for Rest Type
zomato$rest_type <- droplevels(zomato$rest_type)


#:::::::MEASURES OF ASSOSCIATION::::::::

#Chi Square Tests in R for Diiferent Variables with Respect to satisfaction Rate or RATE
#chisq.test() function to perform the test

# With respect to City
table(zomato$Satisfaction_Rate,zomato$listed_in.city.)
chisq.test(zomato$Satisfaction_Rate,zomato$listed_in.city., correct = FALSE) 

# With respect to Online Order
table(zomato$Satisfaction_Rate,zomato$online_order)
chisq.test(zomato$Satisfaction_Rate, zomato$online_order, correct = FALSE) 

#WIth respect to book_table
table(zomato$Satisfaction_Rate,zomato$book_table)
chisq.test(zomato$Satisfaction_Rate,zomato$book_table, correct = FALSE) 


#:::::::::VISUALISATION IN GGPLOTS::::::::::::

#1. Top Restaurants Food Chain in Bangalore
top_restaurant <- zomato %>% select(name) %>% group_by(name) %>% count() %>% arrange(desc(n))
top_restaurant <- top_restaurant[1:10,]

top_restaurant %>%
  ggplot(aes(x=reorder(name,n),y=n))+ 
  geom_bar(stat = "identity", color="black",fill="orange") + 
  coord_flip() +
  labs(title = "Top 10 Food Chains in Bangalore", y="Total Count", x= "Food Chains")
scale_fill_brewer(palette = "Dark2")


#2. Top Locations in Bangalore with Positive Satisfaction Rate
top_location <- zomato %>% filter(Satisfaction_Rate == 'Positive') %>%  select(listed_in.city.) %>% group_by(listed_in.city.) %>% count() %>% arrange(desc(n))
top_location <- top_location[1:15,]

top_location %>%
  ggplot(aes(x=reorder(listed_in.city.,n),y=n))+ 
  geom_bar(stat = "identity", color="black",fill="orange") + 
  coord_flip() +
  labs(title = "Top 15 neighbourhoods with Positive Satisfaction", y="Total Count", x= "Neighbourhoods")
scale_fill_brewer(palette = "Dark2")  

#3. Restaurant Rating Distributions
ggplot(zomato) +
  geom_density(aes(x = rate), fill = '#FFD700') + #geom_histogram
  labs(
    title = 'Restaurant Rating Distributions',
    x = 'Ratings out of 5',
    y = 'Frequency'
  ) +
  ggthemes::theme_few()

#4. Online Order Count
ggplotly(zomato %>% 
           group_by(online_order) %>% 
           summarise(total = n()) %>% 
           ggplot(aes(x=online_order,y=total,fill = online_order))+
           labs(
             title = 'Online Order Count',
             x = 'Online Order?',
             y = 'Total Count'
           )+ 
           geom_bar(stat = 'identity'))


#::::::: NETWORK CENTRALITY FOR LOCATION :::::::

#LOAD IMPORTANT LIBRARIES FOR CENTRALITY
library(ggraph)
library(igraph)
library(echarts4r)
library(ggthemes)

#load the location columns
NetworkEL_loc <- select(zomato,c(10,11))

#Class of NetworkEL_loc
class(NetworkEL_loc)

#Convert it into a Matrix format
Network_Matrix <- as.matrix(NetworkEL_loc)
class(Network_Matrix)

#g is the variable to which we are assigning the network
g <- graph_from_edgelist(Network_Matrix, directed=FALSE)

##if gD should be a directed network
gD <- graph_from_edgelist(Network_Matrix, directed=TRUE)

#simplify the network to remove duplicates.
g <- simplify(g)
g

gD

#Try this out with the two networks (g, and gD) 
Degree <- degree(g)
Indegree.Undirected <- degree(g, mode="in")
Outdegree.Undirected <- degree(g, mode="out")

Degree.Directed <- degree(gD)
Indegree <- degree(gD, mode="in")
Outdegree <- degree(gD, mode="out")

#use the cbind command to combine the measures for comparison
CompareDegree <- cbind(Degree, Indegree.Undirected, Outdegree.Undirected, Degree.Directed, Indegree, Outdegree)

#Eigenvector Centrality
Eig <- evcent(g)$vector

Hub <- hub.score(g)$vector
Authority <- authority.score(g)$vector

#Closeness Centrality
Closeness <- closeness(g)

# Reach at k=2
Reach_2 <- (ego_size(g, 2)-1)/(vcount(g)-1)

## Reach at k=3
Reach_3 <- (ego_size(g, 3)-1)/(vcount(g)-1)

#Betweenness Centrality
Betweenness <- betweenness(g)

centralities <- cbind(Degree, Eig, Hub, Authority, Closeness, Reach_2, Reach_3, Betweenness)

round(cor(centralities), 2)

V(g)$degree <- degree(g)                        # Degree centrality
V(g)$eig <- evcent(g)$vector                    # Eigenvector centrality
V(g)$hubs <- hub.score(g)$vector                # "Hub" centrality
V(g)$authorities <- authority.score(g)$vector   # "Authority" centrality
V(g)$closeness <- closeness(g)                  # Closeness centrality
V(g)$betweenness <- betweenness(g)              # Vertex betweenness centrality

centrality <- data.frame(row.names   = V(g)$name,
                         degree      = V(g)$degree,
                         closeness   = V(g)$closeness,
                         betweenness = V(g)$betweenness,
                         eigenvector = V(g)$eig)

centrality <- centrality[order(row.names(centrality)),]

head(centrality)

# Top ten
head(centrality[order(centrality$betweenness),], n=10)



lay <- layout_with_kk(g)

#Plot Layout
plot(g, layout = lay, 
     vertex.label = NA)

#View the Attributes
View(centrality)

plot.igraph(gD, layout=lay, 
            vertex.size=degree(gD, mode="in"), 
            main="Indegree")

#Write the Centrality Measures in a CSV format
write.csv(centrality, file = "Centrality.csv")

#Modify the Centrality CSV file with average ratings of the location
centrality <- read.csv('centrality.csv')

# Highest Degree Centrality
centrality %>% 
  arrange(desc(degree)) %>% 
  slice(1:10)%>% 
  ggplot(aes(x = fct_reorder(City,degree), y = degree))+
  geom_col(fill = '#e63946')+
  coord_flip() +
  labs(x = '' ,y = '',title = 'City with Highest Degree Centrality') +
  theme_tufte() +
  theme(axis.text = element_text(size = 10),
        plot.title = element_text(size = 15))

#Cities with High Closeness Centrality
centrality %>% 
  arrange(desc(closeness)) %>% 
  slice(1:10)%>% 
  ggplot(aes(x = fct_reorder(City,closeness), y = closeness))+
  geom_col(fill = '#e9c46a')+
  coord_flip() +
  labs(x = '' ,y = '',title = 'Cities with Highest Closeness Centrality') +
  theme_tufte() +
  theme(axis.text = element_text(size = 10),
        plot.title = element_text(size = 15))


#Betweeness Centrality
centrality %>% 
  arrange(desc(betweenness)) %>% 
  slice(1:10)%>% 
  ggplot(aes(x = fct_reorder(City,betweenness), y = betweenness))+
  geom_col(fill = '#2a9d8f')+
  coord_flip() +
  labs(x = '' ,y = '',title = 'Location with high Betweeness Centrality') +
  theme_tufte() +
  theme(axis.text = element_text(size = 10),
        plot.title = element_text(size = 15))


centrality_rating <- read_excel('centrality_rating.xlsx')

# Correlation Between Centrality Measures
cor(centrality_rating$degree,centrality_rating$Average_Rating, method = c("pearson", "kendall", "spearman"))
cor.test(centrality_rating$degree,centrality_rating$Average_Rating, method=c("pearson", "kendall", "spearman"))

cor(centrality_rating$closeness,centrality_rating$Average_Rating, method = c("pearson", "kendall", "spearman"))
cor.test(centrality_rating$closeness,centrality_rating$Average_Rating, method=c("pearson", "kendall", "spearman"))

cor(centrality_rating$betweenness,centrality_rating$Average_Rating, method = c("pearson", "kendall", "spearman"))
cor.test(centrality_rating$betweenness,centrality_rating$Average_Rating, method=c("pearson", "kendall", "spearman"))

cor(centrality_rating$eigenvector,centrality_rating$Average_Rating, method = c("pearson", "kendall", "spearman"))
cor.test(centrality_rating$eigenvector,centrality_rating$Average_Rating, method=c("pearson", "kendall", "spearman"))


#::::: SPLIT THE BANK DATA INTO TRAINING AND TEST::::
#to create a partition with 80%
set.seed(235) #generate a sequence of random numbers
index <- createDataPartition(zomato$Satisfaction_Rate, p = 0.8, list = FALSE,)
train <- zomato[index, ] #first 80% for training 
test <- zomato[-index, ] #bottom 20% for testing

#Formual1 consisting of listed_in.city
formula1 <- Satisfaction_Rate ~ listed_in.city. + rest_type + online_order + book_table + votes + approx_cost.for.two.people.

#Formula consisting of happening_place + Distance_from_Heart
formula <- Satisfaction_Rate ~ happening_place + Distance_from_Heart  + rest_type + online_order + book_table + votes + approx_cost.for.two.people.

#1. ::::::::::: Logistic Regression MODEL::::::::::


#::LR Model1 Consisting of Location Factors::

# Training the Logistic Regression model1
LRM1 <- glm(formula1, data = train, family = "binomial")
LRM1

#Summary of Logistic Regression MODEL
summary(LRM1)

# TRAIN DATA ACCURACY
# Predict test data based on model
LRM_predictions1 <- predict(LRM1,test,type ="response") 

#Convert probabilities to Positive or Negative
LRM_class_pred1<-as.factor(ifelse(LRM_predictions1 > 0.5,"Positive","Negative"))
#evaluate the accuracy of the predictions
postResample(LRM_class_pred1,test$Satisfaction_Rate)

#Confusion Matrix
confmat_log <- table(actual_value = train$Satisfaction_Rate, Predicted_Value = LRM_class_pred1 )
confmat_log


#Logistic Regression Model 2
#::LR Model Consisting of Distance and happening_place::

# Training the Logistic Regression model1
LRM <- glm(formula, data = train, family = "binomial")
LRM

#Summary of Logistic Regression MODEL
summary(LRM)

# TRAIN DATA ACCURACY
# Predict test data based on model
LRM_predictions <- predict(LRM,test,type ="response") 

#Convert probabilities to Positive or Negative
LRM_class_pred<-as.factor(ifelse(LRM_predictions > 0.5,"Positive","Negative"))
#evaluate the accuracy of the predictions
postResample(LRM_class_pred,test$Satisfaction_Rate)

#Confusion Matrix
confmat_log <- table(actual_value = train$Satisfaction_Rate, Predicted_Value = LRM_class_pred )
confmat_log

#Assessing Model R-Square
logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2 ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2 ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2 ", round(R.n, 3), "\n")
}
logisticPseudoR2s(LRM)
#Odds Ratio (Exponential of coefficient) 
exp(LRM$coefficients)

#confidence interval
exp(confint(LRM))

#::Logistic Model ASSUMPTIONS:: 
#Add the predicted probabilities to the data frame
train$predictedProbabilities <- fitted(LRM)

#This shows the probability of churn, and the actual outcome.
head(data.frame(train$predictedProbabilities, train$Satisfaction_Rate))

#Add the standardised and Studentised residuals can be added to the data frame
train$standardisedResiduals <- rstandard(LRM)
train$studentisedResiduals <- rstudent(LRM)

#count the residuals above 1.96
sum(train$standardisedResiduals > 1.96)

#COOKs Distance 
train$cook <- cooks.distance(LRM)
sum(train$cook > 1)

train$leverage <- hatvalues(LRM)

#check if any values are above 0.0009
sum(train$leverage > 0.0009)

library(car)
#VIF to identify if there is a potential problem with multicolinearity
vif(LRM)


#2.::::::: Naives Bayes ::::::

library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
set.seed(1234)

#model 1 without Distance Variable
model1 <- naive_bayes(formula1, data = train, usekernel = T) 
plot(model1) 
p <- predict(model1, train, type = 'prob')
#Confusion Matrix - train data
p1 <- predict(model1, train)
#Confusion Matrix
(tab1 <- table(p1, train$Satisfaction_Rate))
#Accuracy
sum(diag(tab1)) / sum(tab1)
#Confusion Matrix - test data
p2 <- predict(model1, test)
#Confusion Matrix
(tab2 <- table(p2, test$Satisfaction_Rate))
#Accuracy
(sum(diag(tab2))/sum(tab2))


#Model 2 With DIstance from Heart and happening place
formula <- Satisfaction_Rate ~ happening_place + Distance_from_Heart  + rest_type + online_order + book_table + votes + approx_cost.for.two.people.

model <- naive_bayes(formula, data = train, usekernel = T) 
model
plot(model) 
p <- predict(model, train, type = 'prob')

#Confusion Matrix - train data
p1 <- predict(model, train)
#Confusion Matrix
(tab1 <- table(p1, train$Satisfaction_Rate))
#Accuracy
sum(diag(tab1)) / sum(tab1)
#Confusion Matrix - test data
p2 <- predict(model, test)
#Confusion Matrix
(tab2 <- table(p2, test$Satisfaction_Rate))
#Accuracy
(sum(diag(tab2))/sum(tab2))


#3.::::: Random Forest :::::
library(randomForest)
library(datasets)
library(caret)
set.seed(222)
#Model 1 with Location but without dist_from_heart and happening place
rf1 <- randomForest(formula1, data=train, proximity=TRUE) 
rf1
print(rf1)
#Prediction & Confusion Matrix - train data
p1 <- predict(rf1, train)
confusionMatrix(p1, train$Satisfaction_Rate)
#Prediction & Confusion Matrix - test data
p2 <- predict(rf1, test)
confusionMatrix(p2, test$Satisfaction_Rate)
#Error Rate of RF
plot(rf1)

#No. of nodes for the trees
hist(treesize(rf1),
     main = "No. of Nodes for the Trees",
     col = "orange")
#Variable Importance
varImpPlot(rf1,
           sort = T,
           n.var = 10,
           main = "Imporrant Variables")
#MeanDecreaseGini
importance(rf1)


#Model 2 with  dist_from_heart and happening place

rf <- randomForest(formula, data=train, proximity=TRUE) 
rf
print(rf)
#Prediction & Confusion Matrix - train data
p1 <- predict(rf, train)
confusionMatrix(p1, train$Satisfaction_Rate)
#Prediction & Confusion Matrix - test data
p2 <- predict(rf, test)
confusionMatrix(p2, test$Satisfaction_Rate)
#Error Rate of RF
plot(rf)

#No. of nodes for the trees
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "orange")
#Variable Importance
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Imporrant Variables")
#MeanDecreaseGini
importance(rf)


# 4. Extreme gradient boost Tree
# Fit the model on the training set
library(dplyr)
library(caret)
library(xgboost)
library(e1071)  

#Model 1 without happening_place and Heart_of City Paramert instead Listed City is Considered
set.seed(456)
model <- train(
  formula1, data = train, method = "xgbTree",
  trControl = trainControl("cv", number = 2)
)
# Best tuning parameter
model$bestTune


# Make predictions on the test data
predicted.classes <- model %>% predict(test)
head(predicted.classes)

# Compute model prediction accuracy rate
mean(predicted.classes == test$Satisfaction_Rate)

#The function varImp() [in caret] displays the importance of variables in percentage:
varImp(model)

p <- predict(model, train, type = 'prob')
#Confusion Matrix - train data
p1 <- predict(model, train)
#Confusion Matrix
(tab1 <- table(p1, train$Satisfaction_Rate))
#Accuracy
sum(diag(tab1)) / sum(tab1)
#Confusion Matrix - test data
p2 <- predict(model, test)
#Confusion Matrix
(tab2 <- table(p2, test$Satisfaction_Rate))
#Accuracy
(sum(diag(tab2))/sum(tab2))


#Model 2 without location
set.seed(456)
model <- train(
  formula, data = train, method = "xgbTree",
  trControl = trainControl("cv", number = 2)
)
# Best tuning parameter
model$bestTune


# Make predictions on the test data
predicted.classes <- model %>% predict(test)
head(predicted.classes)

# Compute model prediction accuracy rate
mean(predicted.classes == test$Satisfaction_Rate)

#The function varImp() [in caret] displays the importance of variables in percentage:
varImp(model)

p <- predict(model, train, type = 'prob')
#Confusion Matrix - train data
p1 <- predict(model, train)
#Confusion Matrix
(tab1 <- table(p1, train$Satisfaction_Rate))
#Accuracy
sum(diag(tab1)) / sum(tab1)
#Confusion Matrix - test data
p2 <- predict(model, test)
#Confusion Matrix
(tab2 <- table(p2, test$Satisfaction_Rate))
#Accuracy
(sum(diag(tab2))/sum(tab2))


#5. :::: Decision Tree::::::

#Model 1 with location variable
library(DAAG)
library(party)
library(rpart)
library(rpart.plot)
library(mlbench)
library(caret)
library(pROC)
library(tree)
tree <- rpart(formula1, data = train)
rpart.plot(tree)
printcp(tree)


p <- predict(tree, train, type = 'class')
confusionMatrix(p, train$Satisfaction_Rate, positive="Positive")

#ROC Curve
p1 <- predict(tree, test, type = 'prob')
p1 <- p1[,2]
r <- multiclass.roc(test$Satisfaction_Rate, p1, percent = TRUE)
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
         main= 'ROC Curve - Model 1')


#Model 2 with distance from city center and High ACtivity Place
library(DAAG)
library(party)
library(rpart)
library(rpart.plot)
library(mlbench)
library(caret)
library(pROC)
library(tree)
tree <- rpart(formula, data = train)
rpart.plot(tree)
printcp(tree)


p <- predict(tree, train, type = 'class')
confusionMatrix(p, train$Satisfaction_Rate, positive="Positive")

#ROC Curve
p1 <- predict(tree, test, type = 'prob')
p1 <- p1[,2]
r <- multiclass.roc(test$Satisfaction_Rate, p1, percent = TRUE)
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
         main= 'ROC Curve - Model 1')


