

#Instalar librerias
#install.packages("class")
#install.packages("caret")

library(class)
library(caret)


homeCSV <- read.csv("./Data/descargas/train.csv", header = TRUE, sep = "," , stringsAsFactors = FALSE)
homeData <- homeCSV[,(unlist(lapply(homeCSV, is.numeric)))]
homeData$Id <- NULL #eliminamos el ID, que no representa una escala

set.seed(42)

porcentaje <- 60/100
elegidos<-sample(1:nrow(homeData),porcentaje*nrow(homeData))#Muestra aleatoria de numeros de un vector

trainHome<-homeData[elegidos,] #Obtengo las filas de los elementos que estan en el sector de muestra
testHome<-homeData[-elegidos,] #Obtengo las filas de los elementos que no están en el vector de muestra




modeloLinealMulti<-lm(SalePrice~., data = trainHome)
summary(modeloLinealMulti)


#Calculamos la prediccion
prediccion<-predict(modeloLinealMulti,newdata = testHome[1:36])
testHome$PricePred<-prediccion
dif<-abs(testHome$PricePred-testHome$SalePrice)

testSet$mpgPred <-NULL



trainHomeI <- trainHome[,c("SalePrice","MSSubClass", "OverallQual", "OverallCond", "YearBuilt", "MasVnrArea",
                           "X1stFlrSF", "X2ndFlrSF", "BsmtFullBath", "BedroomAbvGr", "GarageCars",
                           "LotFrontage", "KitchenAbvGr", "TotRmsAbvGrd","WoodDeckSF")]
testHomeI <- testHome[,c("SalePrice","MSSubClass", "OverallQual", "OverallCond", "YearBuilt", "MasVnrArea",
                           "X1stFlrSF", "X2ndFlrSF", "BsmtFullBath", "BedroomAbvGr", "GarageCars",
                           "LotFrontage", "KitchenAbvGr", "TotRmsAbvGrd","WoodDeckSF")]


modeloLinealMultiI<-lm(SalePrice~., data = trainHomeI)
summary(modeloLinealMultiI)

#Calculamos la prediccion
prediccion<-predict(modeloLinealMultiI,newdata = testHomeI[2:ncol(testHomeI)])
testHomeI$PricePred<-prediccion
difI<-abs(testHomeI$PricePred-testHomeI$SalePrice)





trainHomeI$PrecioCat[is.na(trainHomeI$SalePrice)] = "minimo"
trainHomeI$PrecioCat[trainHomeI$SalePrice >= 34000 & trainHomeI$SalePrice<178900] = "minimo"
trainHomeI$PrecioCat[trainHomeI$SalePrice>=178900 & trainHomeI$SalePrice<322900] = "bajo"
trainHomeI$PrecioCat[trainHomeI$SalePrice>=322900 & trainHomeI$SalePrice<466900] = "medio"
trainHomeI$PrecioCat[trainHomeI$SalePrice>=466900 & trainHomeI$SalePrice<610900] = "alto"
trainHomeI$PrecioCat[trainHomeI$SalePrice>=610900 & trainHomeI$SalePrice<=755000] = "maximo"

testHomeI$PrecioCat[is.na(testHomeI$SalePrice)] = "minimo"
testHomeI$PrecioCat[testHomeI$SalePrice >= 34000 & testHomeI$SalePrice<178900] = "minimo"
testHomeI$PrecioCat[testHomeI$SalePrice>=178900 & testHomeI$SalePrice<322900] = "bajo"
testHomeI$PrecioCat[testHomeI$SalePrice>=322900 & testHomeI$SalePrice<466900] = "medio"
testHomeI$PrecioCat[testHomeI$SalePrice>=466900 & testHomeI$SalePrice<610900] = "alto"
testHomeI$PrecioCat[testHomeI$SalePrice>=610900 & testHomeI$SalePrice<=755000] = "maximo"

testHomeI[is.na(testHomeI)] <- 0
trainHomeI[is.na(trainHomeI)] <- 0

predKnn<-knn(trainHomeI[,c(2:15)], testHomeI[,c(2:15)], as.factor(trainHomeI$PrecioCat),k=23)

cfm<-confusionMatrix(as.factor(testHomeI$PrecioCat), predKnn)
cfm

testHomeI$Estimacion[testHomeI$PrecioCat == predKnn] = "YES"
table(testHomeI$Estimacion) #Contamos la cantidad de "YES" que hay 

#Con caret usando validacion cruzada
set.seed(42)
trctrl <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 23)


knnTrain <- train(PrecioCat~., data = testHomeI, method = "knn",
                  trControl=trctrl,
                  preProcess = c("center", "scale"), tuneLength=10)

predknn<-predict(knnTrain,newdata = testHomeI[,c(1:16)])
summary(knnTrain)
cfm<-confusionMatrix(as.factor(testHomeI$PrecioCat), predKnn)
cfm


