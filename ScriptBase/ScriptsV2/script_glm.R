library(glmnet)

source('utils/utils_oblig.R')
source('ScriptBase/ScriptsV2/Preprocessing.R')

set.seed(117)

df.thr_vec <- seq(0.1, 0.9, 0.05)

df.fn_summary <- function(data, lev = NULL, model = NULL) {
  fn_summaryUtilityThr(data, df.thr_vec)
}

df.metric <- 'utility'

print('** GLM')

df.glm.ctrl <- trainControl(method = 'cv',
                            number = 5,
                            verboseIter = TRUE,
                            classProbs = TRUE,
                            search = 'random',
                            summaryFunction = df.fn_summary)

df.glm <- train(form = Formula, 
                data = train, 
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

df.glm.dev.prob <- predict(df.glm, newdata = dev, type = 'prob')
df.glm.dev.pred <- fn_pred(df.glm.dev.prob, thr = df.glm.results$prob_thr)

df.glm.dev.utility <- fn_utility(df.glm.dev.pred, dev$Churn)

print(df.glm.dev.utility)

print('Matriz de confusion en dev')

df.glm.dev.cm <- conf_matrix(df.glm.dev.pred,  as.factor(dev$Churn))

print(df.glm.dev.cm)

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    ROC & AUC Computations 
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#ROC 
rl_predict<-as.numeric(df.glm.dev.pred)
rl_prediction<-prediction(rl_predict,dev$Churn)
roc(rl_prediction)

#AUC
rl_AUC <- auc(rl_prediction)*100
rl_AUC