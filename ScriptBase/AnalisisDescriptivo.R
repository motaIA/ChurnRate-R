##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                        Churn Rate Predictor  
##                          Danny J. Mota 
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
