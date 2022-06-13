##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##      Tecnicas de Machine Learning - Universidad ORT del Uruguay
##                              OBLIGATORIO
##   Estudiantes: Ignacio Cerde√±a (No. - ) y Danny J. Mota (No. 264796) 
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

df.output.levels <- levels(df$Churn)    #No sÈ que quiere ver con esto

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

#creo objeto para verificar la utilidadc

df.fn_summary <- fn_summaryUtility

df.metric <- 'utility'

#Tomo todas las variables
df.form <- Churn ~ .

print('** Utilidad maxima en dev')

#Compara lo mismo para tener la m·xima utilidad

df.max_utility <- fn_utility(df.part$dev$Churn, df.part$dev$Churn)  #VER ESTO, HAY 2 $

print(df.max_utility)

print('** Baseline -- todos Yes')

df.baseline.pred <- factor(rep('Yes', nrow(df.part$dev)), 
                           levels = df.output.levels)

print('Utilidad de la prediccion en dev')

df.baseline.utility <- fn_utility(df.baseline.pred, df.part$dev$Churn)

print(df.baseline.utility)

print('Matriz de confusion')

df.baseline.cm <- conf_matrix(df.baseline.pred, df.part$dev$Churn)  #ERROR VER

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
##                Regresion Lineal
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::




#Genero una base para modificar los Yes por 1, y los NO por 0
df.rl<-df
df.rl<-df.rl%>%
  mutate(Churn=ifelse(Churn=="No",0,1))



# lista para guardar las regresiones
reg <- list()


#Lista de regresiones, hipotesis
#reg[[1]] <- glm(Churn ~ ., data = df.rl)   #Cambiar a 0 y 1??
reg[[2]] <- glm(Churn ~ TotalRecurringCharge, data = df.rl)



##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                  PARTE 3 - An√°lisis de error
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



#Lista de las formulas utilizadas para generar las hipotesis 
#Aca irian las pruebas de nuestras hipÛtesis, por ahora solo es "."
prueba.form <- c('TotalRecurringCharge')


#lista de objetos de formula
list_fit <- glm_fit_formulas(train = df.rl, formulas =prueba.form)












