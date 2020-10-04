###############
# Prediction Model of Survivals in Titanic
# Competition of Kaggle
#
# Creator : Igna Garcia
# In representation of : Universidad Nacional del Oeste, Argentina
#
# Create date: 2020/10/03
# Update date: 2020/10/30 
#        comment: 
###############


##----------------START LIBRARIES
library(readr) #To read .csv
library(dplyr) #To use select(), subset(), mutate(), and others
##----------------END LIBRARIES


##----------------START READ DATA
train <- read_csv("train.csv")
# Survived-> 0-No; 1-Yes
# Pclass-> 1-Upper; 2-Middle; 3-Lowe;
# SibSp-> number of siblings  or spouses on family relation
# Parch-> number of parents or childrens on family relation
# Fare-> price of ticket
##----------------END READ DATA


##----------------START PROCESS DATA
str(train)
summary(train)
sapply(train, function(x) sum(is.na(x)))

# Sex-> 1-male; 2-female;
train$Sexo <- 0
train <- train %>% mutate(Sexo = case_when(.$Sex == "male" ~ 1,
                                           .$Sex == "female" ~ 2))
# Embarked-> 1-Cherbourg; 2-Queenstown; 3-Southampton; 
train$Embark <- 0
train <- train %>% mutate(Embark = case_when(.$Embarked == 'C' ~ 1,
                                             .$Embarked == 'Q' ~ 2,
                                             .$Embarked == 'S' ~ 3))
# -Name; -Ticket; -Cabin; -Sex; -Embarked; 
train <- select(train, -c("Name", "Cabin", "Sex", "Embarked"))


data1 <- train[,c(1:3,5:8,10,12)]


##----------------END PROCESS DATA
  

##----------------START MODELS
  
##----------------END MODELS


##----------------START PREDICTION

##----------------END PREDICTION