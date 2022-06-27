##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##      Tecnicas de Machine Learning - Universidad ORT del Uruguay
##                  Proyecto Final - Churn Rate Predictor  
##   Estudiantes: Ignacio Cerdeña (No.171385) y Danny J. Mota (No.264796) 
##                      Semestre Marzo 2022 
##                            NaiveBayes.R
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source('utils/utils_oblig.R')

set.seed(117)

dataset <- read.csv('data/dataset.csv')

str(dataset)

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    Ingeneria de Datos
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

print('** Tratamiento inicial de los datos')

as.data.frame(sort(names(dataset)))

lapply(dataset,summary)

data.frame(colSums(is.na(dataset)))

#contar ceros??

#Modificamos Churn a 1 y 0
dataset <- dataset %>%
  mutate(Churn = ifelse(Churn == 'Yes', 1, 0))
df$Churn <- as.factor(df$Churn)

rownames(dataset) <- dataset$CustomerID

df <- na.omit(dataset[,-1])
df$ServiceArea <- NULL

df.output.levels <- levels(df$Churn)

print('** Distribucion a-priori de la variable a predecir')

df.apriori <- prop.table(table(df$Churn))
  
df$random <- sample(0:1,size = nrow(df),replace = T,prob = c(0.3,0.7)) 

train<-filter(df,random==1)
test<-filter(df,random==0)
df$random <- NULL

formula_NB <-  Churn ~ .

formula_GLM <- 


##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    GLM
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

formula_rl <- formula
rl<- glm(formula_rl,train,family=binomial(link='logit'))
summary(rl)

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    NAIVE BAYES
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


NB=naiveBayes(formula, data=train)
print(NB)
