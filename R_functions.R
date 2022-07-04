score_model <- function(tree, testdata){
  prediction <- predict(tree, testdata)
  prob <- predict(tree, testdata, type = "prob")
  # Calculate the overall accuracy.
  correct_ctree <- prediction == testdata$fibroids_us
  # Extract the class probabilities.
  probabilities <- 1 - prob[,1]
  # Plot the performance of the model applied to the evaluation set as
  # an ROC curve.
  pred <- prediction(probabilities, testdata$fibroids_us)
  perf <- performance(pred,"tpr","fpr")
  
  auc <- performance(pred,"auc")
  auc_ROCR <- performance(pred, measure = "auc")
  auc_ROCR <- auc_ROCR@y.values[[1]]
  par(mar=c(1,1,1,1)) #исправление ошибки
  
  return(auc_ROCR)
}
