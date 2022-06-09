if(!require(caret)) install.packages('caret')
library(caret)

# https://topepo.github.io/caret/index.html

# utility

fn_utility <- function(yhat, y) { 
  fp_utility <- -10 * ( yhat == 'Yes' & y == 'No' )
  tp_utility <- 25.5 * ( yhat == 'Yes' & y == 'Yes' )
  return(mean(fp_utility + tp_utility)) 
}

fn_summaryUtility <- function(data, lev = NULL, model = NULL) {
  utility <- fn_utility(yhat = data$pred, y = data$obs)
  c(utility = utility)
}

# utility threshold

fn_pred <- function(data, thr = 0.5) {
  factor(ifelse(data[, 'Yes'] > thr, 'Yes', 'No'))
}

fn_summaryUtilityThr <- function(data, thr_vec) {
  utility <- c()
  for(thr in thr_vec){
    pred_thr <- fn_pred(data, thr)
    uty <- fn_utility(yhat = pred_thr, y = data$obs)
    utility <- c(utility, uty)
  }
  c(utility = max(utility), prob_thr = thr_vec[which.max(utility)])
}

fn_results <- function(model) {
  utility <- model$results$utility
  prob_thr <- model$results$prob_thr[which.max(utility)]
  return(list(utility = max(utility), prob_thr = prob_thr))
}

# particion

train_dev_partition <- function(df, p = 0.7) {
  size <- round(nrow(df) * p)
  train_idx <- sample.int(nrow(df), size)
  train <- df[train_idx,]
  dev  <- df[-train_idx,]
  return(list(train = train, dev = dev))
}

# matriz de confusion

conf_matrix <- function(y_pred, y_real, positive = 'Yes') {
  cm <- confusionMatrix(data = y_pred, 
                        reference = y_real, 
                        positive = positive, 
                        mode = 'prec_recall')
  return(cm$table)
}

# output

gen_prediction <- function(model, test, prob_thr = 0.5, id = '') {
  prob <- predict(model, newdata = test, type = 'prob', na.action = na.pass)
  pred <- fn_pred(prob, thr = prob_thr) 
  gen_output(test, pred, id)
}

gen_output <- function(test, pred, id) {
  pred <- ifelse(pred == 'No', FALSE, TRUE)
  output <- data.frame(CustomerID = rownames(test), Churn = pred)
  filename <- paste0(c('test_sample_pred', '(', id, ').csv'), collapse = '')
  write.csv(output, file = filename, row.names = FALSE) 
}

# plot

plot_thr_utility <- function(thr_utility, thr_vec, main = '') {
  plot(thr_utility, 
       type = 'l', 
       col = 'red', 
       main = main, 
       ylab = 'Utilidad', 
       xlab = 'Umbral', 
       xaxt = 'n')
  axis(1, 
       at = seq(1, length(thr_vec)), 
       labels = thr_vec)
}
