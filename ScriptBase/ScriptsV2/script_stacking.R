source('ScriptBase/ScriptsV2/Preprocessing.R')
source('utils/utils_oblig.R')

if(!require('caretEnsemble')) install.packages("caretEnsemble")
library(caretEnsemble)
if(!require('xgbTree')) install.packages("xgbTree")
library('xgbTree')
library('gbm')
library('caret')
set.seed(117)

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    ENSEMBLE-STACKING  
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

thr_vec <- seq(0.1, 0.9, 0.05)

fn_summary <- function(data, lev = NULL, model = NULL) {
  fn_summaryUtilityThr(data, thr_vec)
}

###APLICAR GRID SEARCH 
###CUIDAR EL OVERFITTING 
###REVISAR BIEN LOS PARAMETROS Y TUNEAR!!!!!

algoritmos <- c("glm", "glmnet", "xgbTree", "gbm","rpart")

trainctr <- trainControl(method = 'repeatedcv',
                         number = 5,
                         verboseIter = TRUE,
                         classProbs = TRUE, 
                         search = 'random', 
                         summaryFunction = fn_summary)

modelos <- caretList(Formula, data = train, trControl = trainctr, methodList = algoritmos)

stacking <- caretStack(modelos, method= 'glm', trControl = trainctr)

summary(stacking)

prediccion_stacking <- predict(stacking ,dev)

matriz_stacking <- confusionMatrix(as.factor(dev$Churn), prediccion_stacking)

matriz_stacking

Utility_stacking <- fn_utility(prediccion_stacking,dev$Churn)
Utility_stacking

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##              Generacion de la prediccion sobre test sample 
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

print('** Generacion de la prediccion sobre test sample')

test_sample <- read.csv('data/test_sample.csv')
rownames(test_sample) <- test_sample$CustomerID
test_sample$CustomerID <- NULL
test_sample$ServiceArea <- NULL

script.name <- 'stacking'
script.date <- date()

file_id <- paste0(c(script.name, script.date), collapse = ' ')

gen_prediction(stacking, test_sample, prob_thr = 0.29, id = file_id)

print('Done')

#result <-  read.csv('test_sample_pred(rpart Sun Jun 26 21:51:37 2022).csv')
#nrow(result)
