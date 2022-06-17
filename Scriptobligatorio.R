##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##      Tecnicas de Machine Learning - Universidad ORT del Uruguay
##                              OBLIGATORIO
##   Estudiantes: Ignacio Cerdeña (No. - ) y Danny J. Mota (No. 264796) 
##                      Semestre Marzo 2022 
##                            script.R
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


library(tidyverse)
library(tidyr)
library(readxl)
library(dplyr)

if(!require('glmnet')) install.packages('glmnet')
if(!require('caret')) install.packages('caret')

library(glmnet)
library(caret)
library(ggcorrplot)
install.packages("corrplot")
library(corrplot)

library(e1071)




#############################
#####Script de Baseline######
#############################


source('utils_oblig.R')

set.seed(117)

script.name <- 'baseline'

script.date <- date()

script.start <- Sys.time()

print('Start')

# leer el archivo dataset.csv de la carpeta

dataset <- read.csv('dataset.csv')

print('** Tratamiento inicial de los datos')

# ver la estructura del dataset


str(dataset)

# asignar el nombre del jugador como nombre de la fila

rownames(dataset) <- dataset$CustomerID

df <- na.omit(dataset[,-1])
df$ServiceArea <- NULL

#AGREGO UN AS.FACTOR
df$Churn<-as.factor(df$Churn)

df.output.levels <- levels(df$Churn)    #No s? que quiere ver con 



##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                Analisis Descriptivo sobre df
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



#Vemos en porcentajes cuantos No y Yes tenemos

print('** Distribucion a-priori de la variable a predecir')

df.apriori <- prop.table(table(df$Churn))

print(df.apriori)


#Veamos las distribuciones por si hay que normalizar

hist(df$MonthlyRevenue)    #No es normal
hist(df$MonthlyMinutes)   #No es normal
hist(df$TotalRecurringCharge)  #Mas parecido a normal
hist(df$DirectorAssistedCalls)  #No es normal
hist(df$OverageMinutes)   #No es normal
hist(df$RoamingCalls)      #No es normal
hist(df$PercChangeMinutes)   #Normal
hist(df$PercChangeRevenues)   #No es normal
hist(df$DroppedCalls)   #No es normal
hist(df$BlockedCalls)   #No es normal
hist(df$UnansweredCalls)  #No es normal
hist(df$CustomerCareCalls)  #NO es normal
hist(df$ThreewayCalls)  #NO es normnal
hist(df$ReceivedCalls)  #No es normal
hist(df$OutboundCalls)  #No es normal
hist(df$InboundCalls)   #No es normal
hist(df$PeakCallsInOut)   #No es normal
hist(df$OffPeakCallsInOut)  #No es normal
hist(df$DroppedBlockedCalls)  #No es normal
hist(df$CallForwardingCalls)   #No es normal
hist(df$CallWaitingCalls)   #No es normal
hist(df$MonthsInService)  #No es normal
hist(df$UniqueSubs) #No es normal
hist(df$ActiveSubs) #No es normal
hist(df$Handsets)   #No es normal
hist(df$HandsetModels)   #No es normal
hist(df$CurrentEquipmentDays)  #Parecido a una normal
hist(df$AgeHH1)   #NO es normal
hist(df$AgeHH2)  #Sacando los 0 puede ser normal
hist(df$RetentionCalls)  #No es normal
hist(df$RetentionOffersAccepted) #No es normal
hist(df$ReferralsMadeBySubscriber) #No es normal
hist(df$IncomeGroup)  #No es normal
hist(df$AdjustmentsToCreditRating) #No es normal
  



#Particion del dataset para train

df.part <- train_dev_partition(df, p = 0.9)

#creo objeto para verificar la utilidadd

df.fn_summary <- fn_summaryUtility

df.metric <- 'utility'

#Tomo todas las variables
df.form <- Churn ~ .

print('** Utilidad maxima en dev')

#Compara lo mismo para tener la m?xima utilidad

df.max_utility <- fn_utility(df.part$dev$Churn, df.part$dev$Churn)  

print(df.max_utility)

print('** Baseline -- todos Yes')

df.baseline.pred <- factor(rep('Yes', nrow(df.part$dev)), 
                           levels = df.output.levels)

print('Utilidad de la prediccion en dev')

df.baseline.utility <- fn_utility(df.baseline.pred, df.part$dev$Churn)

print(df.baseline.utility)

print('Matriz de confusion')

df.baseline.cm <- conf_matrix(df.baseline.pred, df.part$dev$Churn)  

print(df.baseline.cm)

print('** Generacion de la prediccion sobre test sample')

test_sample <- read.csv('../data/test_sample.csv')
rownames(test_sample) <- test_sample$CustomerID
test_sample$CustomerID <- NULL
test_sample$ServiceArea <- NULL

test_sample.pred <- factor(rep('Yes', nrow(test_sample)), 
                           levels = df.output.levels)

file_id <- paste0(c(script.name, script.date), collapse = ' ')

gen_output(test_sample, test_sample.pred, file_id)

print('Done')

script.done <- Sys.time()

print(script.done - script.start)



##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                Regresion Logistica
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#Genero una base para modificar los Yes por 1, y los NO por 0

df.rl<-df

#df.rl<-df.rl%>%
  #mutate(Churn=ifelse(Churn=="No",0,1))

df.rl$Churn<-ifelse(df.rl$Churn=="Yes",1,0)



#Veo la estructura de los datos
str(df)

#Tengo que eliminar las variables character para poder hacer una matriz de correlación
df.rl1<-df.rl[ ,1:54]
df.rl2<-df.rl1[ ,-53]
df.rl3<-df.rl2[ ,-52]
df.rl4<-df.rl3[,-51:-52]
df.rl5<-df.rl4[ ,-49]
df.rl6<-df.rl5[ ,-45:-46]
df.rl7<-df.rl6[ ,-31:-42]

df.rl<-df.rl7





#Particion de df


df.ntrain <- ceiling(nrow(df.rl) * .8)
df.ntest <- nrow(df.rl) - df.ntrain

h.part <- partition_train_test(df.rl, ntrain = df.ntrain) # ver utils.R
h.train <- h.part$train
h.test <- h.part$test


#Matriz de correlacion

corrplot(cor(df.rl),method="circle")


# Aprender la hipotesis
rl.fit <- glm(Churn ~ ., data =h.train , family = 'binomial')


# Calcular la prediccion (train)
rl.proba <- predict(rl.fit, newdata = h.train, type = 'response')
rl.yhat <- ifelse(rl.proba > 0.3, 1, 0)


# Plotear la distribucion (histograma) de la prediccion en train
plot(as.factor(rl.yhat), 
     main = 'Prediccion en train', 
     ylab = 'quality',
     xlab = 'observacion',
     col = 'red')   


# Error (train)
print('Error Regresion Logistica Train')
h.pred2_train_error <- fn_err_cla(rl.yhat, h.train$Churn)
print(h.pred2_train_error)


# Calcular la prediccion (test)
rl.proba2 <- predict(rl.fit, newdata = h.train, type = 'response')
rl.yhat2 <- ifelse(rl.proba2 > 0.3, 1, 0)


# Plotear la distribucion (histograma) de la prediccion en test
plot(as.factor(rl.yhat2), 
     main = 'Prediccion en test', 
     ylab = 'quality',
     xlab = 'observacion',
     col = 'red')    


# Error (test)
print('Error Regresion Logistica Test')
h.pred2_test_error <- fn_err_cla(rl.yhat2, df.rl$Churn)
print(h.pred2_test_error)


# Metricas de utilidad
fn_accuracy <- function(yhat, y) {mean(yhat == y)}

print('Accuracy train:')

rl_accuracy_train <- fn_accuracy(rl.yhat, df.rl$Churn)
print(rl_accuracy_train)

print('Accuracy test:')
rl_accuracy_test <- fn_accuracy(rl.yhat2, df.rl$Churn)
print(rl_accuracy_test)


# Matriz de confusion

rl_cm_test <- confusionMatrix(as.factor(rl.yhat2),as.factor(h.train$Churn))   
erroryhat<-levels(rl.yhat)
classyhat<-class(rl.yhat)

print("Matriz de confusion Regresion Logistica:")
print(list(tab = rl_cm_test$table,
           acc = rl_cm_test$overall['Accuracy'],
           pre = rl_cm_test$byClass['Precision'],
           rec = rl_cm_test$byClass['Recall'],
           f1 = rl_cm_test$byClass['F1']))

