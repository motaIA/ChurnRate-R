##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##      Tecnicas de Machine Learning - Universidad ORT del Uruguay
##                  Proyecto Final - Churn Rate Predictor  
##   Estudiantes: Ignacio Cerdeña (No.171385) y Danny J. Mota (No.264796) 
##                        Semestre Marzo 2022 
##                          Preprocessing.R
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
library(dplyr)

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    Preprocessing 
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#Set seed para replicabilidad 
set.seed(117)

#Cargamos el dataset 
dataset <- read.csv('data/dataset.csv')

print('** Tratamiento inicial de los datos')

#Estructura del dataset 
str(dataset)

rownames(dataset) <- dataset$CustomerID
dataset$CustomerID <- NULL
dataset$ServiceArea <- NULL

# Normalizar 
preprocessParams <- preProcess(dataset, method=c("range"))
print(preprocessParams)
normalizado <- predict(preprocessParams, dataset)
summary(normalizado)

#cargamos df omitiendo los NAs
df <- na.omit(normalizado,stringsAsFactors=FALSE)

col_factores <- c('ChildrenInHH', 'HandsetRefurbished', 'HandsetWebCapable', 'TruckOwner', 'RVOwner', 'BuysViaMailOrder',
                  'RespondsToMailOffers', 'OptOutMailings', 'NonUSTravel', 'HasCreditCard', 'NewCellphoneUser', 'OwnsComputer',
                  'OwnsMotorcycle', 'Homeownership', 'MaritalStatus', 'MadeCallToRetentionTeam','NotNewCellphoneUser', 'CreditRating','PrizmCode', 'Occupation' )

##Convertir todas las variables en factores 
df[,col_factores] <- lapply(df[,col_factores],as.factor)

df.output.levels <- levels(df$Churn)

print('** Distribucion a-priori de la variable a predecir')

df.apriori <- prop.table(table(df$Churn))

#Train/Test Split 
trainIndex <- createDataPartition(df$Churn, p = .9, list = FALSE,times = 1)
train <- df[ trainIndex,]
dev  <- df[-trainIndex,]

#Establecer formula
Formula <- Churn ~ .
