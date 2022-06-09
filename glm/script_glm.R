library(glmnet)

source('../utils/utils_oblig.R')

set.seed(117)

script.name <- 'glm'

script.date <- date()

script.start <- Sys.time()

print('Start')

# leer el archivo dataset.csv de la carpeta

dataset <- read.csv('../data/dataset.csv')

# ver la estructura del dataset

# str(dataset)

# asignar el nombre del jugador como nombre de la fila

rownames(dataset) <- dataset$CustomerID

df <- na.omit(dataset[,-1])

df$ServiceArea <- NULL

print('** Distribucion a-priori de la variable a predecir')

print(prop.table(table(df$Churn)))

df.part <- train_dev_partition(df, p = 0.9)

df.fn_summary <- fn_summaryUtility

df.metric <- 'utility'

df.form <- Churn ~ .

print('** GLM')

df.glm.ctrl <- trainControl(method = 'cv',
                            number = 5,
                            verboseIter = TRUE,
                            search = 'random',
                            summaryFunction = df.fn_summary)

df.glm <- train(form = df.form, 
                data = df.part$train, 
                method = 'glmnet', 
                family = 'binomial',
                trControl = df.glm.ctrl,
                tuneLength = 10,
                metric = df.metric)

print(df.glm)

plot(df.glm)

df.glm.model <- df.glm$finalModel

df.glm.model.coef <- predict(df.glm.model, 
                             s = df.glm.model$lambdaOpt, 
                             type = 'coefficients')
  
print(df.glm.model.coef)

df.glm.pred <- predict(df.glm, newdata = df.part$dev)

print('Utilidad')

df.glm.utility <- fn_utility(df.glm.pred, df.part$dev$Churn)

print(df.glm.utility)

print('Matriz de confusion')

df.glm.cm <- conf_matrix(df.glm.pred, df.part$dev$Churn)

print(df.glm.cm)


print('** Generacion de la prediccion sobre test sample')

test_sample <- read.csv('../data/test_sample.csv')
rownames(test_sample) <- test_sample$CustomerID
test_sample$CustomerID <- NULL
test_sample$ServiceArea <- NULL

file_id <- paste0(c(script.name, script.date), collapse = ' ')

gen_prediction(df.glm, test_sample, id = file_id)

print('Done')

script.done <- Sys.time()

print(script.done - script.start)

# [1] "Utilidad real de la prediccion"
# [1] 0.1644182
