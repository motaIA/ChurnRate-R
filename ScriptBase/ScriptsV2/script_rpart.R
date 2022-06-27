library(rpart)
library(rpart.plot)

source('utils/utils_oblig.R')
source('ScriptBase/ScriptsV2/Preprocessing.R')

set.seed(117)

df.thr_vec <- seq(0.2, 0.6, 0.05)

df.fn_summary <- function(data, lev = NULL, model = NULL) {
  fn_summaryUtilityThr(data, df.thr_vec)
}

df.metric <- 'utility'

print('** RPART')

df.rpart.ctrl <- trainControl(method = 'cv',
                              number = 10,
                              verboseIter = TRUE,
                              classProbs = TRUE,
                              search = 'random',
                              summaryFunction = df.fn_summary)

df.rpart <- train(form = Formula, 
                  data = train, 
                  method = 'rpart', 
                  trControl = df.rpart.ctrl,
                  tuneLength = 5,
                  metric = df.metric)

print(df.rpart)

plot(df.rpart)

df.rpart.model <- df.rpart$finalModel

rpart.plot(df.rpart.model)

df.rpart.results <- fn_results(df.rpart)

print('Umbral')

print(df.rpart.results$prob_thr)

print('Utilidad en train')

print(df.rpart.results$utility)

print('Utilidad en dev')

df.rpart.dev.prob <- predict(df.rpart, newdata = dev, type = 'prob')
df.rpart.dev.pred <- fn_pred(df.rpart.dev.prob, thr = df.rpart.results$prob_thr)

df.rpart.dev.utility <- fn_utility(df.rpart.dev.pred, dev$Churn)

print(df.rpart.dev.utility)

print('Matriz de confusion en dev')

df.rpart.dev.cm <- conf_matrix(df.rpart.dev.pred, as.factor(dev$Churn))

print(df.rpart.dev.cm)

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    ROC & AUC Computations 
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#ROC
ar_predict<-as.numeric(df.nb.dev.pred)
ar_prediction <- prediction(ar_predict,dev$Churn)
roc(ar_prediction)

#AUC
ar_AUC <- auc(ar_prediction)*100
ar_AUC

#Utility_AR <- fn_utility(ar_predict,test)
