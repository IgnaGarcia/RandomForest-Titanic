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
library(ranger) #To Random Forest optimised
library(randomForest) 
##----------------END LIBRARIES


start <- Sys.time()
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

#Divide on train and test
smp_size <- floor(0.70 * nrow(data))
set.seed(123)
train_aux <- sample(seq_len(nrow(data)), size = smp_size)
#Train set and Test set
train <- data[train_aux, ]
test <- data[-train_aux, ]

# Set whitout NA
set1 <- na.omit(train)
summary(set1)

# Set whit Age NA = mean
set2 <- train 
set2$Age[is.na(set2$Age)] <- mean(set2$Age, na.rm=T)
summary(set2)

# Set whit Age as numeric factor
set3 <- train %>% mutate(Age = case_when(.$Age <= 10 ~ 1,
                                        .$Age <= 20 ~ 2,
                                        .$Age <= 30 ~ 3,
                                        .$Age <= 40 ~ 4,
                                        .$Age <= 50 ~ 5,
                                        .$Age <= 60 ~ 6,
                                        .$Age <= 70 ~ 7,
                                        .$Age <= 80 ~ 8,
                                        TRUE ~ 0))
##----------------END PROCESS DATA
  

##----------------START MODELS

#Function to RF
cores <- 4 #number of cores
registerDoParallel(cores = cores)

par <- function(model, numbTrees, numbVars){
  foreach(ntree.iter = rep(ceiling(numbTrees / cores), cores), 
          .combine = combine, .packages = "randomForest, ranger") %dopar% { 
            ranger( Survived ~ . , data= set1[,-1]
                    , num.trees = numbTrees
                    , mtry = numbVars
                    , importance = "impurity"
                    , write.forest = T
                    , probability = T
                    , alpha = 0.005
            )
          }
}

accuracy=c(0)
runs = 10
qvar = 3

for (qvar in 2:7) {
  for(j in 1: runs){
    
    entrenamiento <- ozono[train_ind, ]
    test          <- ozono[-train_ind, ]
    modelo.forest <- par(entrenamiento, 600,qvar)  
    prediccion <- predict(modelo.forest, test, type='class') 
    
    mc <- with(test,table(prediccion, test$clase))  # Matriz de Confusion
    
    #Calculo el % de  aciertos totales
    aciertos <- sum(diag(mc)) / sum(mc) * 100
    vAciertos_RF[j] = aciertos
  }
  cat( '\nNVariables: ', qvar)
  cat(': Promedio: ',mean(vAciertos_RF),"% ")
  cat( ' ( min: ', min(vAciertos_RF))
  cat( ' ; max: ', max(vAciertos_RF), ") \n")
}
##----------------END MODELS


##----------------START PREDICTION
predm1s1 <- predict(m1s1, data = test)
##----------------END PREDICTION

totalTime <- Sys.time() - start
print(totalTime)