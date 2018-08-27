# install.packages("class")
# install.packages("caret")
library(class)
library(caret)

datos<-mtcars

#Particionando los datos en conjunto de entrenamiento y prueba

set.seed(123)

porciento <- 70/100 #Porciento en el que se partirán los datos
muestra<-sample(1:nrow(datos),porciento*nrow(datos))#Muestra aleatoria de numeros de un vector

trainSet<-datos[muestra,] #Obtengo las filas de los elementos que estan en el sector de muestra
testSet<-datos[-muestra,] #Obtengo las filas de los elementos que no están en el vector de muestra


#-------------------------------------------------
# Regresi�n Lineal Simple 
#-------------------------------------------------

#Predecir la cantidad de millas por gal�n usando el peso del carro

#Generaci�n del modelo
modeloLinealSimple<-lm(mpg~wt, data = trainSet)
summary(modeloLinealSimple)

#predicci�n
prediccion<-predict(modeloLinealSimple,newdata = testSet[,2:ncol(testSet)])

# Se agrega la predicci�n al conjunto de entrenamiento
testSet$mpgPred<-prediccion

#Ver la diferencia entre lo real y lo predicho
dif<-abs(testSet$mpgPred-testSet$mpg)

#Hay que establecer la diferencia m�nima para saber quienes son los mejores clasificados

testSet$mpgPred <-NULL
#-------------------------------------------------
# Regresi�n Lineal Multivariada
#-------------------------------------------------

#Generaci�n del modelo
modeloLinealMulti<-lm(mpg~., data = trainSet)
summary(modeloLinealMulti)
#quizas no es necesario tener todas las variables


#predicci�n
prediccion<-predict(modeloLinealMulti,newdata = testSet[,2:ncol(testSet)])

# Se agrega la predicci�n al conjunto de entrenamiento
testSet$mpgPred<-prediccion
#Ver la diferencia entre lo real y lo predicho
dif<-abs(testSet$mpgPred-testSet$mpg)
testSet$mpgPred <-NULL

#-------------------------------------------------
# Regresi�n Log�stica
#-------------------------------------------------

#Para predecir si la transmisi�n de un carro ser� mec�nica o autom�tica
modRegLog<-glm(formula = am~.,data = trainSet)
summary(modRegLog)
predLogRegProb<-predict(modRegLog,newdata = testSet[,c(1:8,10:11)])
predLogReg<-ifelse(predLogRegProb>=0.5,1,0)

#Generando la matriz de confusi�n, para esto hay que cargar el paquete caret
cfm<-confusionMatrix(as.factor(testSet$am),as.factor(predLogReg))
cfm
#-------------------------------------------------
# KNN
#-------------------------------------------------

#Para predecir si la transmisi�n de un carro ser� mec�nica o autom�tica
#Con class
library(class)
predKnn<-knn(trainSet[,c(1:8,10:11)],testSet[,c(1:8,10:11)],as.factor(trainSet$am),k=3)
cfm<-confusionMatrix(as.factor(testSet$am),predKnn)
cfm

#Con caret usando validaci�n cruzada
set.seed(123)
trctrl <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 3)

trainSet$am<-as.factor(trainSet$am)
testSet$am<-as.factor(testSet$am)

knnTrain <- train(am ~., data = trainSet, method = "knn",
                  trControl=trctrl,
                  preProcess = c("center", "scale"), tuneLength=10)
predknn<-predict(knnTrain,newdata = testSet[,c(1:8,10:11)])
summary(knnTrain)
cfm<-confusionMatrix(as.factor(testSet$am),predKnn)
cfm





