##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##      Tecnicas de Machine Learning - Universidad ORT del Uruguay
##                  Proyecto Final - Churn Rate Predictor  
##   Estudiantes: Ignacio Cerdeña (No.171385) y Danny J. Mota (No. 264796) 
##                      Semestre Marzo 2022 
##                            script.R
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
source('utils/utils_oblig.R')

set.seed(117)

script.name <- 'baseline'

script.date <- date()

script.start <- Sys.time()

print('Start')

# leer el archivo dataset.csv de la carpeta

dataset <- read.csv('data/dataset.csv')

length(which(dataset$Churn=='Yes'))

length(which(dataset$Churn=='No'))

print('** Tratamiento inicial de los datos')

# ver la estructura del dataset
# str(dataset)

# asignar el nombre del jugador como nombre de la fila

rownames(dataset) <- dataset$CustomerID

df <- na.omit(dataset[,-1])
df$ServiceArea <- NULL

df.output.levels <- levels(df$Churn)

print('** Distribucion a-priori de la variable a predecir')

df.apriori <- prop.table(table(df$Churn))

print(df.apriori)

df.part <- train_dev_partition(df, p = 0.9)

df.fn_summary <- fn_summaryUtility

df.metric <- 'utility'

df.form <- Churn ~ .

print('** Utilidad maxima en dev')

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


