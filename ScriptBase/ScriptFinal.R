##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##      Tecnicas de Machine Learning - Universidad ORT del Uruguay
##                  Proyecto Final - Churn Rate Predictor  
##   Estudiantes: Ignacio Cerdeña (No.171385) y Danny J. Mota (No.264796) 
##                      Semestre Marzo 2022 
##                            ScriptFinal.R
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
source('utils/utils_oblig.R')

#librerias 
library(tidyverse)
library(tidyr)
library(readxl)
library(dplyr)

if(!require('glmnet')) install.packages('glmnet')
if(!require('caret')) install.packages('caret')
if(!require('caret')) install.packages("corrplot")
if(!require('ROCR')) install.packages("ROCR")
if(!require('randomForest')) install.packages("randomForest")
if(!require('rpart')) install.packages("rpart")
if(!require('rpart.plot')) install.packages("rpart.plot")

install.packages('gbm')                    # for fitting the gradient boosting model
install.packages('naivebayes') 
install.packages('caret')                 # for general data preparation and model fitting
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%

library(gbm)
library(caret)                
library(naivebayes)


library(glmnet)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(caret)
library(ggcorrplot)
library(corrplot)
library(e1071)

#Set seed para replicabilidad 
set.seed(117)

#Cargamos el dataset 
dataset <- read.csv('data/dataset.csv')
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    Ingeneria de Datos
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

print('** Tratamiento inicial de los datos')

#Estructura del dataset 
str(dataset)

as.data.frame(sort(names(dataset)))

lapply(dataset,summary)

#Cuantos valores NA tiene el dataset?
data.frame(colSums(is.na(dataset)))

#Modificamos Churn a 1 y 0
dataset <- dataset %>% mutate(Churn = ifelse(Churn == 'Yes', 1, 0))

df$Churn <- as.factor(df$Churn)

rownames(dataset) <- dataset$CustomerID

df <- na.omit(dataset[,-1])
df$ServiceArea <- NULL

df.output.levels <- levels(df$Churn)

print('** Distribucion a-priori de la variable a predecir')

df.apriori <- prop.table(table(df$Churn))
  
df$random <- sample(0:1,size = nrow(df),replace = T,prob = c(0.3,0.7)) 

train<-filter(df,random==1)
test<-filter(df,random==0)
df$random <- NULL
 
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    FORMULAS 
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
formula_NB <-  Churn ~ .

formula_GLM <- Churn ~ .

formula_RF <- Churn ~ .

formula_AR <- Churn ~ .

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    GLM
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    NAIVE BAYES
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

print('** Implementacion de Naive Bayes')

NB=naiveBayes(formula_NB, data=train)
print(NB)

Bayes_predict <- predict(NB, test)

#mod
plot(Bayes_predict~test$Churn, border=9, main="Comparación Churn real vs Prediccion - NaiveBayes", xlab="Churn real en la muestra test", ylab="Previsión de Churn", col=c('Darkblue','DarkGreen'))

BayesMatrizConfusion <- table(test$Churn, Bayes_predict)

confusionMatrix(BayesMatrizConfusion,positive = '1', mode = 'prec_recall')

#ROC 
Bayes_predict<-as.numeric(Bayes_predict)
Bayes_prediction<-prediction(Bayes_predict,test$Churn)
roc(Bayes_prediction)

#AUC
Bayes_AUC <- auc(Bayes_prediction)*100
Bayes_AUC

#UNIFICAR TODAS LAS METRICAS EN UN DATAFRAME PARA HACER LA COMPARATIVA

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    RANDOM FOREST 
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

rf<-randomForest(formula_RF,train,importance=T)
rf

test$Churn <- as.factor(test$Churn)

rf_predict<-predict(rf,test,type = 'response')
confusion(test$Churn,rf_predict,'0.25')
plot(rf_predict~test$Churn, border=9, col=11, main="Comparación Churn real vs Scoring - Modelo Random Forest",cex.main=0.9,xlab="Churn real en la muestra test", ylab="Scoring del Churn", cex.axis=0.7, cex.lab=0.8)
umb_rf<-umbrales(test$Churn,rf_predict)

rf_metricas<-filter(umb_rf,umbral=='0.25')
rf_metricas

rf_prediction<-prediction(rf_predict,test$Churn)
roc(rf_prediction)

rf_metricas<-cbind(rf_metricas,AUC=round(auc(rf_prediction),2)*100)
print(t(rf_metricas))

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    Arboles de Decision 
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

ar<-rpart(formula_AR, train, method = 'class', parms = list(
  split = "information"), 
  control = rpart.control(cp = 0.0015))

printcp(ar)

plotcp(ar)

rpart.plot(ar,type=2,extra = 7, under = TRUE,under.cex = 0.8,fallen.leaves=F,gap = 0,cex=0.4,yesno = 2,box.palette = "GnYlRd",branch.lty = 3)

#rpart.rules(ar,style = 'tall',cover = T)

ar_predict<-predict(ar,test,type = 'prob')[,2]

#matriz de confusion
conf<-table(test$Churn,ar_predict>=0.25)

metricas<-function(matriz_conf){
  acierto <- (matriz_conf[1,1] + matriz_conf[2,2]) / sum(matriz_conf) *100
  precision <- matriz_conf[2,2] / (matriz_conf[2,2] + matriz_conf[1,2]) *100
  cobertura <- matriz_conf[2,2] / (matriz_conf[2,2] + matriz_conf[2,1]) *100
  F1 <- 2*precision*cobertura/(precision+cobertura)
  resultado<-c(acierto,precision,cobertura,F1)
  return(resultado)
}

ar_metricas<-metricas(conf)
ar_metricas 

#ROC
ar_prediction <- prediction(ar_predict,test$Churn)
roc(ar_prediction)

#AUC
ar_metricas<-cbind(ar_metricas,AUC=round(auc(ar_prediction),2)*100)
print(t(ar_metricas))


##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    BOOSTING 
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

traingbm <- data.frame(train)

##HACER TODAS LAS VARIABLES AS FACTORS

model_gbm = gbm(Churn ~ .,
                data = traingbm ,
                distribution = "multinomial",
                cv.folds = 10,
                shrinkage = .01,
                n.minobsinnode = 10,
                n.trees = 500)       # 500 tress to be built

summary(model_gbm)

#use model to make predictions on test data
pred_test = predict.gbm(object = model_gbm,
                        newdata = test,
                        n.trees = 500,           # 500 tress to be built
                        type = "response")

pred_test

# Give class names to the highest prediction value.
class_names = colnames(pred_test)[apply(pred_test, 1, which.max)]
result = data.frame(test$Species, class_names)

print(result)

conf_mat = confusionMatrix(test$Species, as.factor(class_names))
print(conf_mat)

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    ENSEMBLE-STACKING  
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



