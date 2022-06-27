library(glmnet)

source('utils/utils_oblig.R')
source('ScriptBase/ScriptsV2/Preprocessing.R')

set.seed(117)

df.thr_vec <- seq(0.1, 0.9, 0.05)

df.fn_summary <- function(data, lev = NULL, model = NULL) {
  fn_summaryUtilityThr(data, df.thr_vec)
}

df.metric <- 'utility'

print('** Naive Bayes')

df.nb.ctrl <- trainControl(method = 'cv',
                            number = 5,
                            verboseIter = TRUE,
                            classProbs = TRUE,
                            search = 'random',
                            summaryFunction = df.fn_summary)

df.nb <- train(form = Formula, 
                data = train, 
                method = 'naive_bayes', 
                trControl = df.nb.ctrl,
                tuneLength = 5,
                metric = df.metric)

print(df.nb)

plot(df.nb)

df.nb.model <- df.nb$finalModel

df.nb.results <- fn_results(df.nb)

print('Umbral')

print(df.nb.results$prob_thr)

print('Utilidad en train')

print(df.nb.results$utility)

print('Utilidad en dev')

df.nb.dev.prob <- predict(df.nb, newdata = dev, type = 'prob')
df.nb.dev.pred <- fn_pred(df.nb.dev.prob, thr = df.nb.results$prob_thr)

df.nb.dev.utility <- fn_utility(df.nb.dev.pred, dev$Churn)

print(df.nb.dev.utility)

print('Matriz de confusion en dev')

df.nb.dev.cm <- conf_matrix(df.nb.dev.pred,  as.factor(dev$Churn))

print(df.nb.dev.cm)

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    ROC & AUC Computations 
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#ROC 
Bayes_predict<-as.numeric(df.nb.dev.pred)
Bayes_prediction<-prediction(Bayes_predict,dev$Churn)
roc(Bayes_prediction)

#AUC
Bayes_AUC <- auc(Bayes_prediction)*100
Bayes_AUC
