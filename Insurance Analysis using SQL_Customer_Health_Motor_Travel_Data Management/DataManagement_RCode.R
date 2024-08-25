#Install the required packages
#Read the Packages
library(dplyr)
library(tidyverse)

#Set Working Directory
setwd('D:/Business Analytics/Data Management')

#Read the excel sheet into variable
cust <- read.csv('D:/Business Analytics/Data Management/Assignment 1/customer.csv')
motor <- read.csv('D:/Business Analytics/Data Management/Assignment 1/motor_policies.csv')
health <- read.csv('D:/Business Analytics/Data Management/Assignment 1/health_policies.csv')
travel <- read.csv('D:/Business Analytics/Data Management/Assignment 1/travel_policies.csv')

#:: Creation of Analytical Base Table ::

#Change Column Names. Make it same as the other tables for motorID, heatlthID,travelID 
colnames(cust)[12] <- 'motorID'
colnames(cust)[13] <- 'healthID'
colnames(cust)[14] <- 'travelID'

#Change the names of policyStart and policyEnd for all the tables
colnames(health)[3] <- 'HealthPolicyStart'
colnames(health)[4] <- 'HealthPolicyEnd'
colnames(motor)[4] <- 'MotorPolicyStart'
colnames(motor)[5] <- 'MotorPolicyEnd'
colnames(travel)[2] <- 'TravelPolicyStart'
colnames(travel)[3] <- 'TravelPolicyEnd'

#Join 2 tables and then join all the 4 tables at last
cust_health <- left_join(cust,health,"healthID") 
cust_health_motor <- left_join(cust_health,motor,"motorID" )       
cust_health_motor_travel <- left_join(cust_health_motor,travel,"travelID")
#cust_health_motor_travel for all the 4 tables

#Remove all the unwanted Columns, and change the veh_value by multiplying by 10000
Insurance <- cust_health_motor_travel %>% #Extracting the value from cust_health_motor_travel TABLE
  distinct() %>%
  select(-`Title`,-`GivenName`,-`MiddleInitial`,-`Surname`,`Occupation`,-`MotorType`,-`clm`,-`numclaims`,-`v_body`,-`LastClaimDate`) %>%
  mutate(veh_value = veh_value*10000)
#Analytical Base Table named 'Insurance' is created

#::::: Data Quality Issues and Action ::::::

##Re-summarize CardType 0 as 'Other Mode of Transaction'
Insurance$CardType[Insurance$CardType == 0] <- "Other Mode of Transaction"
#Convert CardType in factor
Insurance$CardType <- as.factor(Insurance$CardType)


#Re-summarize Gender as 'Male' WHERE Gender = 'male' or 'm'
Insurance$Gender[(Insurance$Gender == 'male')] <- 'Male'
Insurance$Gender[(Insurance$Gender == 'm')] <- 'Male'
#Re-summarize Gender as 'Female' WHERE Gender = 'female' or 'f'
Insurance$Gender[(Insurance$Gender == 'female')] <- 'Female'
Insurance$Gender[(Insurance$Gender == 'f')] <- 'Female'
#Convert Gender into Factor
Insurance$Gender <- as.factor(Insurance$Gender)

#Re-summarize ComChannel as 'Email' where ComChannel = 'E'
Insurance$ComChannel[Insurance$ComChannel == 'E'] <- 'Email'
#Re-summarize ComChannel as 'SMS' where ComChannel = 'S'
Insurance$ComChannel[Insurance$ComChannel == 'S'] <- 'SMS'
#Re-summarize ComChannel as 'Phone' where ComChannel = 'P'
Insurance$ComChannel[Insurance$ComChannel == 'P'] <- 'Phone'
#Convert ComChannel into factor
Insurance$ComChannel <- as.factor(cust$ComChannel)

#Re-summarize Age to 41 (average age 41.38) for the age above 100 or negative
Insurance$Age[Insurance$Age > 100] <- 41
Insurance$Age[Insurance$Age < 0] <- 41

#Re-summarize DependentsKids in table Health_policies to 4 from 40
Insurance$DependentsKids[Insurance$DependentsKids == 40] <- 4
#Convert DependentsKids into Factor
Insurance$DependentsKids <- as.factor(health$DependentsKids)

#Re-summarize veh_value 0 and above 130000 to median value 15100\
Insurance$veh_value[Insurance$veh_value > 130000] <- 15100
Insurance$veh_value[Insurance$veh_value == 0] <- 15100

#Convert HealthDependentsAdults into Factor
Insurance$HealthDependentsAdults <- as.factor(Insurance$HealthDependentsAdults)

#Convert HealthType into factor
Insurance$HealthType <- as.factor(health$HealthType)

#Convert MotorType into factor
Insurance$MotorType <- as.factor(Insurance$MotorType)

#Convert Vehicle Age into factor
Insurance$v_age <- as.factor(Insurance$v_age)

#Convert Travel type into factor
Insurance$TravelType <- as.factor(Insurance$TravelType)

#GGPLOT GRAPH For Representing Vehicle Price Outliers
Insurance %>% ggplot(aes(x=Insurance$v_age,y=Insurance$veh_value, color = Gender, na.rm = TRUE))+
  geom_boxplot()+
  labs(title = "Comparison of Motor Price (USD) and Age of Vehicle (Years)", x="Age of Motor", y= "Vehicle Price ($)", fill = "Gender")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
scale_fill_brewer(palette = "Dark2")


