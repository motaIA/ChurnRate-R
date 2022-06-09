library(glmnet)

source('../utils/utils_oblig.R')

set.seed(117)

script.name <- 'glm_prob'

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

df.glm.ctrl <- trainControl(method = 'none',
                            verboseIter = TRUE,
                            classProbs = TRUE,
                            search = 'random',
                            summaryFunction = df.fn_summary)

df.glm <- train(form = df.form, 
                data = df.part$train, 
                method = 'glmnet', 
                family = 'binomial',
                trControl = df.glm.ctrl,
                tuneLength = 1,
                metric = df.metric)

print(df.glm)

df.glm.prob <- predict(df.glm, newdata = df.part$dev, type = 'prob')

df.thr_vec <- seq(0.1, 0.9, 0.025)
df.thr_utility <- rep(0, length(df.thr_vec))
for(i in 1:length(df.thr_vec)) {
    pred <- fn_pred(df.glm.prob, thr = df.thr_vec[i]) 
    df.thr_utility[i] <- fn_utility(yhat = pred, y = df.part$dev$Churn)
}

print('Utilidad por umbral')

print(df.thr_utility)

plot_thr_utility(df.thr_utility, df.thr_vec, 'glmnet')
