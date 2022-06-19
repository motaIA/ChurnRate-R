##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##      Tecnicas de Machine Learning - Universidad ORT del Uruguay
##                  Proyecto Final - Churn Rate Predictor  
##   Estudiantes: Ignacio Cerdeña (No.171385) y Danny J. Mota (No. 264796) 
##                      Semestre Marzo 2022 
##                            NaiveBayes.R
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source('utils/utils_oblig.R')

set.seed(117)


dataset <- read.csv('data/dataset.csv')


##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    Ingeneria de Datos
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


print('** Tratamiento inicial de los datos')