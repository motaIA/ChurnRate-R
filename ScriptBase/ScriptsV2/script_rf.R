library(glmnet)
library(randomForest)
#library(mlbench)
library(caret)
library(e1071)

source('utils/utils_oblig.R')
source('ScriptBase/ScriptsDiscretos/Preprocessing.R')

set.seed(117)

df.thr_vec <- seq(0.1, 0.9, 0.05)

df.fn_summary <- function(data, lev = NULL, model = NULL) {
  fn_summaryUtilityThr(data, df.thr_vec)
}

df.metric <- 'utility'

df.form <- Churn ~ .

print('** Naive Bayes')

df.rf.ctrl <- trainControl(method = 'cv',
                           number = 5,
                           verboseIter = TRUE,
                           classProbs = TRUE,
                           search = 'random',
                           summaryFunction = df.fn_summary)

df.rf <- train(form = Formula, 
               data = train, 
               method = 'rf', 
               trControl = df.rf.ctrl,
               tuneLength = 5,
               metric = df.metric)

print(df.rf)

plot(df.rf)

df.rf.model <- df.rf$finalModel

df.rf.model.coef <- predict(df.rf.model, 
                            s = df.rf.model$lambdaOpt, 
                            type = 'coefficients')

print(df.rf.model.coef)

df.rf.results <- fn_results(df.rf)

print('Umbral')

print(df.rf.results$prob_thr)

print('Utilidad en train')

print(df.rf.results$utility)

print('Utilidad en dev')

df.rf.dev.prob <- predict(df.rf, newdata = dev, type = 'prob')
df.rf.dev.pred <- fn_pred(df.rf.dev.prob, thr = df.rf.results$prob_thr)

df.rf.dev.utility <- fn_utility(df.rf.dev.pred, dev$Churn)

print(df.rf.dev.utility)

print('Matriz de confusion en dev')

df.rf.dev.cm <- conf_matrix(df.rf.dev.pred,   as.factor(dev$Churn))

print(df.rf.dev.cm)

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    ROC & AUC Computations 
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#ROC
#ar_predict<-as.numeric(df.nb.dev.pred)
#r_prediction <- prediction(ar_predict,dev$Churn)
#roc(ar_prediction)

#AUC
#ar_AUC <- auc(ar_prediction)*100
#ar_AUC
