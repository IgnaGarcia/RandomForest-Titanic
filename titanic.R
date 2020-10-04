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
data <- read_csv("train.csv")
# Survived: 0-No; 1-Yes
# Pclass: 1-Upper; 2-Middle; 3-Lowe;
# SibSp: number of siblings  or spouses on family relation
# Parch: number of parents or childrens on family relation
# Fare: price of ticket
##----------------END READ DATA


##----------------START PROCESS DATA
str(data)
summary(data)
sapply(data, function(x) sum(is.na(x)))

# Survived to factor
data$Survived <- as.factor(data$Survived)

# Sex: 1-male; 2-female;
data$Sexo <- 0
data <- data %>% mutate(Sexo = case_when(.$Sex == "male" ~ 1,
                                         .$Sex == "female" ~ 2))

# Embarked: 1-Cherbourg; 2-Queenstown; 3-Southampton; 
data$Embark <- 0
data <- data %>% mutate(Embark = case_when(.$Embarked == 'C' ~ 1,
                                           .$Embarked == 'Q' ~ 2,
                                           .$Embarked == 'S' ~ 3))

# Less columns: -Name; -Ticket; -Cabin; -Sex; -Embarked; 
data <- select(data, -c("Name", "Ticket","Cabin", "Sex", "Embarked"))

# NAs treatment
data <- subset(data, !is.na(data$Embark))

# Set whitout NA
set1 <- na.omit(data)
summary(set1)

# Set whit Age NA = mean
set2 <-  data 
set2$Age[is.na(set2$Age)] <- mean(set2$Age, na.rm=T)
summary(set2)
##----------------END PROCESS DATA
  

##----------------START MODELS
  
##----------------END MODELS


##----------------START PREDICTION

##----------------END PREDICTION