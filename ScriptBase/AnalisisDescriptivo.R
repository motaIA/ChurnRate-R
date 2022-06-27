##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##      Tecnicas de Machine Learning - Universidad ORT del Uruguay
##                  Proyecto Final - Churn Rate Predictor  
##   Estudiantes: Ignacio Cerdeña (No.171385) y Danny J. Mota (No. 264796) 
##                      Semestre Marzo 2022 
##                        AnalisisDescriptivo.R
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
source('utils/utils_oblig.R')

set.seed(117)

script.name <- 'baseline'

script.date <- date()

script.start <- Sys.time()

print('Start')

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    Analisis Descriptivo
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

dataset <- read.csv('data/dataset.csv')

length(which(dataset$Churn=='Yes'))

length(which(dataset$Churn=='No'))

print('** Tratamiento inicial de los datos')
