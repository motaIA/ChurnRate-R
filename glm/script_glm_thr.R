library(glmnet)

source('../utils/utils_oblig.R')

set.seed(117)

script.name <- 'glm_thr'

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

df.thr_vec <- seq(0.1, 0.9, 0.05)

df.fn_summary <- function(data, lev = NULL, model = NULL) {
  fn_summaryUtilityThr(data, df.thr_vec)
}

df.metric <- 'utility'

df.form <- Churn ~ .

print('** GLM')

df.glm.ctrl <- trainControl(method = 'cv',
                            number = 5,
                            verboseIter = TRUE,
                            classProbs = TRUE,
                            search = 'random',
                            summaryFunction = df.fn_summary)

df.glm <- train(form = df.form, 
                data = df.part$train, 
                method = 'glmnet', 
                family = 'binomial',
                trControl = df.glm.ctrl,
                tuneLength = 5,
                metric = df.metric)

print(df.glm)

plot(df.glm)

df.glm.model <- df.glm$finalModel

df.glm.model.coef <- predict(df.glm.model, 
                             s = df.glm.model$lambdaOpt, 
                             type = 'coefficients')
  
print(df.glm.model.coef)

df.glm.results <- fn_results(df.glm)

print('Umbral')

print(df.glm.results$prob_thr)

print('Utilidad en train')

print(df.glm.results$utility)

print('Utilidad en dev')

df.glm.dev.prob <- predict(df.glm, newdata = df.part$dev, type = 'prob')
df.glm.dev.pred <- fn_pred(df.glm.dev.prob, thr = df.glm.results$prob_thr)

df.glm.dev.utility <- fn_utility(df.glm.dev.pred, df.part$dev$Churn)

print(df.glm.dev.utility)

print('Matriz de confusion en dev')

df.glm.dev.cm <- conf_matrix(df.glm.dev.pred, df.part$dev$Churn)

print(df.glm.dev.cm)

print('** Generacion de la prediccion sobre test sample')

test_sample <- read.csv('../data/test_sample.csv')
rownames(test_sample) <- test_sample$CustomerID
test_sample$CustomerID <- NULL
test_sample$ServiceArea <- NULL

file_id <- paste0(c(script.name, script.date), collapse = ' ')

gen_prediction(df.glm, test_sample, prob_thr = df.glm.results$prob_thr, id = file_id)

print('Done')

script.done <- Sys.time()

print(script.done - script.start)

# [1] "Utilidad real de la prediccion"
# [1] 0.9698145
