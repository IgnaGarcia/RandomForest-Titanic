# Arbol de Descicion con Ozono

#### Importar Bibliotecas
library(randomForest) # para  el Modelo RF
library(caret)  
library(corrplot)  
library(C50)          # para  el Modelo C50
library(rpart)        # para el Modelo rpart
library(rpart.plot)
library(party)
library(RCurl)
library(doParallel)  # para la concurrencia
library(readxl)
library(reshape)

#### Funcion de ejecucion en Paralelo
cores <- 4 #nro de nucleos a usar en modo paralelo
registerDoParallel(cores = cores)

par <- function(unModelo, cantArboles, cantVars){
  foreach(ntree.iter = rep(ceiling(cantArboles / cores), cores), # divido la cantidad total de arboles que tengo que generar por la cantidad de nucleos
          .combine = combine, .packages = "randomForest") %dopar% { #uno los bosques creados por cada nucleo
            randomForest( clase ~ .
                          , data= unModelo       # datos para entrenar 
                          , ntree= ntree.iter    # cantidad de arboles
                          , mtry=cantVars        # cantidad de variables
                          , replace = T            # muestras con reemplazo
                          , importance=T        # para poder mostrar la importancia de cada var
                          , class = NULL
            )
          }
}


#### Lectura
ozono <- read_excel("ozono.xls", na = "?")


#### Preparacion de los Datos
ozono <- na.omit(ozono) #Elimino na

ozono$clase = as.factor(ozono$clase) #Convierto a factor

ozono <- ozono[,-1]

############################################################################
####--------------Inicio del Random Forest--------------#####
####----------Inicio del Primer experimento----------

### Datos de Prueba y Entrenamiento
#  xx% para el conjunto de entrenamiento:
smp_size <- floor(0.80 * nrow(ozono))

# Seteo semilla de los datos
set.seed(123)

# Creo el Set de Entrenamiento
train_ind <- sample(seq_len(nrow(ozono)), size = smp_size)
entrenamiento <- ozono[train_ind, ]

# Creo el Set de Prueba
test          <- ozono[-train_ind, ]


### Visualizo el OOB
#X11()
tuneRF(x = entrenamiento,   # data set de entrenamiento
       y = entrenamiento$clase,  # variable a predecir
       mtryStart  = 1,     # cantidad de variables inicial
       stepFactor = 2,     # incremento de variables
       ntreeTry   = 400,    # cantidad arboles a ejecutar en cada iteracion
       improve    = 0.01    # mejora minina del OOB para seguir iteraciones
) # esto dio como resultado que entre 2 y 4 var se decremente el OOB = 5.2% y 3%

# el ciclo es para ajustar la cantidad de variables y arboles
vAciertos_RF=c(0)
for(j in 1: 8){
  gc()
  modelo.forest <- par(entrenamiento, 400,j)
  
  # Crea prediccion y Matriz de confusion:
  prediccion <- predict(modelo.forest, test, type='class'); # Matriz de Confusion
  mc <- with(test,table(prediccion, test$clase))
  
  #Calculo el % de  aciertos totales
  aciertos_RF <- sum(diag(mc)) / sum(mc) * 100
  cat("\nCantidad de variables:",j)
  cat("\nCorrectamente clasificados:",round(aciertos_RF,4),"%\n")
  vAciertos_RF[j] = aciertos_RF
}
vAciertos_RF

####----------Fin del Primer experimento----------

############################################################################

####----------Inicio del Segundo experimento----------

# Creo diferentes conjuntos de entrenamiento y text y ejecuto el random forest
vAciertos_RF=c(0)
corridas = 10
qvar = 3

#Desde : Hasta variables
for (qvar in 2:8) {
  #Repeticiones por cada cantidad de variables
  for(j in 1:corridas){
    gc()
  
    # conjuntos de entrenamiento y prueba:
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

# aciertos con 80% 20%: ( tomando 400 arboles y 10 corridas)
# NVariables:  2: Promedio:  94.59459 %  ( min:  94.59459 ; max:  94.59459 ) 
# NVariables:  3: Promedio:  94.59459 %  ( min:  94.59459 ; max:  94.59459 ) 
# NVariables:  4: Promedio:  94.72973 %  ( min:  94.59459 ; max:  94.86486 ) 
# NVariables:  5: Promedio:  94.81081 %  ( min:  94.59459 ; max:  95.13514 ) 
# NVariables:  6: Promedio:  94.81081 %  ( min:  94.59459 ; max:  95.13514 ) 
# NVariables:  7: Promedio:  94.89189 %  ( min:  94.59459 ; max:  95.13514 ) 
# NVariables:  8: Promedio:  94.81081 %  ( min:  94.32432 ; max:  95.13514 ) 


#### Importancia de Variables
#X11(10)
importancia = data.frame(importance(modelo.forest))
importancia <- sort_df(importancia, vars = 'MeanDecreaseGini')
varImpPlot(modelo.forest, sort = T, n.var = 18 , main = 'Top 18 importancia de las variables')

round(importance(modelo.forest), 2 )

####----------Fin del Segundo experimento----------

####--------------Fin del Random Forest--------------####

#### C5.0
library(C50)
vAciertos_c50=c(0)
for(j in 1: 20){
  gc()
  train_ind <- sample(seq_len(nrow(ozono)), size = smp_size)
  
  # conjuntos de entrenamiento y prueba:
  entrenamiento <- ozono[train_ind, ]
  test          <- ozono[-train_ind, ]
  modelo.c50 <- C5.0(clase ~ ., data=entrenamiento)
  prediccion <- predict(modelo.c50, test, type="class")
  mc <- with(test,table(prediccion, test$clase))  # Matriz de Confusion
  cat("**Iteracion ", j,"\n")
  
  # Aciertos en %
  acierto_c50 <- sum(diag(mc)) / sum(mc) * 100
  #cat("\n numero de corrida:",j,"\n",round(acierto_c50,2),"%\n\n\n")
  vAciertos_c50[j] = acierto_c50
}

cat('promedio: ',mean(vAciertos_c50),"% ",'( min: ', min(vAciertos_c50), ', max: ', max(vAciertos_c50), ")\n")
# aciertos de 20 corridas:    promedio:  91.97297 %  ( min:  88.37838 , max:  93.78378 )

#### Fin C5.0


#### SVM   
library(e1071)
vAciertos_svm=c(0)
for(j in 1: 50){
  gc()
  train_ind <- sample(seq_len(nrow(ozono)), size = smp_size)
  
  # conjuntos de entrenamiento y prueba:
  entrenamiento <- ozono[train_ind, ]
  test          <- ozono[-train_ind, ]
  modelo.svm <- svm(clase ~ ., data=entrenamiento)
  pred <- predict(modelo.svm, test, type="class")
  
  # Matriz de confusion
  mc <- table(pred,test$clase, dnn = c("Asignado","Real"))
  
  #cat("** SVM\n")
  # Aciertos en %
  acierto_svm <- sum(diag(mc)) / sum(mc) * 100
  
  # cat("\n numero de corrida:",j,"\n",round(acierto_svm,2),"%\n\n\n")
  vAciertos_svm[j] = acierto_svm
}

# vAciertos_svm # veo los resultados todos juntos
cat('promedio: ',mean(vAciertos_svm),"% ",
    '( min: ', min(vAciertos_svm), 
    ', max: ', max(vAciertos_svm), ")\n")
# aciertos con 80-20: 91.65% ( promedio de 20 corridas ; min es 83 y max es 97)

#### Fin SVM


# AdaBoost - Adaptative Boosting
#  TARDA MUCHO
library(adabag)
modelo.ad <- boosting(clase ~., data=entrenamiento)
pred <- predict(modelo.ad, test, type="class")

#Matriz de confusion

mc <- table(pred,test$clase, dnn = c("Asignado","Real"))
#mc <- table(pred, test[, pred$clase], dnn = c("Asignado","Real"))

print(mc)

aciertos_ada <- sum(diag(mc)) / sum(mc) * 100
cat("\nCorrectamente clasificados:",round(aciertos_ada,2),"%\n\n\n")
# 90.81% de aciertos

##########################################################################
# Clasificador bayesiano ingenuo (Naive Bayes classifier): naiveBayes

modelo.nb  <- naiveBayes(  clase ~., data=entrenamiento)
pred <- predict(modelo.nb, test, type="class")
pred
# Matriz de confusión
mc <- table(pred, test$clase, dnn = c("Asignado","Real"))

# Ordenar tabla alfabéticamente
# Por algún motivo a veces sale desordenada
mc <- mc[order(rownames(mc)),order(colnames(mc))]
cat("** Clasificador bayesiano ingenuo: naiveBayes\n"); print(mc)
aciertos10 <- sum(diag(mc)) / sum(mc) * 100
cat("\nCorrectamente clasificados:",round(aciertos10,2),"%\n\n\n")

# 62.7% aciertos

#################################
# Modelo rp  - modelo de arbol: usando rpart
# PASO 1:   Crea Arbol de Decision
# ---------------------------------------------------------------------------
library(rpart)
library(rpart.plot)
ModeloArbol<-rpart(clase ~ .,data=entrenamiento, parms=list(split="information"))
print(ModeloArbol)
rpart.plot(ModeloArbol) ##  aqui tengo el resultado graficamente en entrenamiento
rpart.plot(ModeloArbol, type=1, extra=100,cex = .7,
           box.col=c("gray99", "gray88")[ModeloArbol$frame$yval])
# ---------------------------------------------------------
library(nnet)
vAciertos_nn=c(0)
for(j in 1: 20){
  gc()
  train_ind <- sample(seq_len(nrow(ozono)), size = smp_size)
  entrenamiento <- ozono[train_ind, ]
  test          <- ozono[-train_ind, ]
  parametros <- train(clase ~ ., data=entrenamiento, method="nnet", trace=F)
  size <- parametros$bestTune$size
  decay <- parametros$bestTune$decay
  
  modelo.nnet <- nnet(clase ~ .,trace= F, data=entrenamiento, size=size, decay=decay)
  pred.nnet <- predict(modelo.nnet, test, type="class")
  
  mc <- table(pred.nnet,test$clase, dnn = c("Asignado","Real"))
  mc <- mc[order(rownames(mc)),order(colnames(mc))]
  #  cat("** Red neuronal: nnet\n") ; print(mc)
  aciertos <- sum(diag(mc)) / sum(mc) * 100
  #  cat("\nro de corrida:",j,"\n")
  #  cat("\nCorrectamente clasificados:",round(aciertos,2),"%\n\n\n")
  vAciertos_nn[j] = aciertos
}
cat('promedio: ',mean(vAciertos_nn),"% ",' ; min: ', min(vAciertos_nn),' ; max: ', max(vAciertos_nn), "\n")
pred.nnet

