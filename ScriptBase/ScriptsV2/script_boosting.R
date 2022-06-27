source('ScriptBase/ScriptsV2/Preprocessing.R')
source('utils/utils_oblig.R')
library('gbm')
set.seed(117)
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    ENSEMBLE-BOOSTING 
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
df_boost <- na.omit(normalizado) 

trainIndex2 <- createDataPartition(df_boost$Churn, p = .9, list = FALSE,times = 1)
train_boost <- df[ trainIndex2,]
dev_boost  <- df[-trainIndex2,]

# Cambiar churn a 0 1 para usar otro tipo de distribucion 
traingbm <- data.frame(train_boost)

traingbm <- traingbm %>% mutate(Churn = ifelse(Churn == 'Yes', 1, 0))

# Experimentar con distribution?shrinkage? 
model_gbm = gbm(Formula,
                data = traingbm ,
                distribution = "adaboost",
                cv.folds = 5,   #folds de cross validation 
                n.minobsinnode = 10,
                n.trees = 10000)     #numero de arboles 

summary(model_gbm)

dev <- dev %>% mutate(Churn = ifelse(Churn == 'Yes', 1, 0))

pred_test = predict.gbm(object = model_gbm,
                        newdata = dev,
                        n.trees = 1000,           
                        type = "response")

pred_test

prueba <- ifelse(pred_test > 0.29, 1, 0)

Utility_Boosting <- fn_utility_num(prueba, dev$Churn)
Utility_Boosting