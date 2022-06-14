library("xlsx")
library("mice")
library("VIM")
library("dplyr")
library("cowplot")
library("ggplot2")
library("plyr")
library("png")
library("caret")
library("party")
library("partykit")
library("stablelearner")
library("ROCR")
library("PRROC")
library("magrittr")

#ФУНКЦИИ ДЛЯ ТРЕНИРОВКИ МОДЕЛЕЙ
define_grids <- function(dataframe){
  gridParameters_ctree <<- expand.grid(mtry = c(0,2,4,6),
                                       maxdepth = c(2,3,4),
                                       mincriterion = c(0.7,0.75,0.8,0.85,0.9,0.95),
                                       #teststat = c("quad", "max"),
                                       stringsAsFactors = FALSE
                                       #testtype = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic"),
                                       # mincriterion = c(0.5, 0.7, 0.9, 0.95),
                                       # #replace = c(TRUE, FALSE),
                                       # #fraction = 
                                       # maxdepth = c(2,3,4)
                                       # #minbucket = 
                                       # #minsplit = )
)}

define_controlTree <- function(grid){
  control_ctree <<- party::ctree_control(mincriterion = grid['mincriterion'],
                                         mtry = grid['mtry'],
                                         maxdepth = grid['maxdepth'])
}

build_ctree <- function(data, weights, seed) {
  if (!is.null(seed)) set.seed(seed)
  tree <<- party::ctree(fibroids_us ~., data, controls = control_ctree, weights = weights)
}

plot_ctree <- function(ctree){
  plot(ctree)
}

out_info <- function(n_tree, tree){
  cat("Номер дерева: ", n_tree,"\n")
  cat("seed: ", seeds[n_tree],"\n")
  cat("mtry: ", gridParameters_ctree[n_tree, 'mtry'],"\n")
  cat("maxdepth: ", gridParameters_ctree[n_tree, 'maxdepth'],"\n")
  cat("mincriterion: ", gridParameters_ctree[n_tree, 'mincriterion'],"\n")
  cat("Задействованные переменные: \n")
  print(tree@data@formula$input)
}

score_model <- function(tree, testdata){
  prediction <- predict(tree, testdata)
  # Calculate the overall accuracy
  correct_ctree <- prediction == testdata$fibroids_us
  
  # Extract the class probabilities.
  probabilities <- 1 - unlist(treeresponse(tree, newdata=testdata), use.names=F)[seq(1,nrow(testdata)*2,2)]
  
  # Plot the performance of the model applied to the evaluation set as
  # an ROC curve.
  pred <- prediction(probabilities, testdata$fibroids_us)
  perf <- performance(pred,"tpr","fpr")
  auc <- performance(pred,"auc")
  auc_ROCR <- performance(pred, measure = "auc")
  auc_ROCR <- auc_ROCR@y.values[[1]]
  par(mar=c(1,1,1,1)) #исправление ошибки
  
  # print(table(prediction))
  # print(table(testing$fibroids_us))
  # print(paste("% правильных прогнозов", mean(correct_ctree)))
  # cat("AUC = ", auc_ROCR, "\n")
  # 
  # plot(perf, main="ROC curve")
  # abline(a = 0, b = 1)
  return(auc_ROCR)
}

predict_fibroids <- function(row_value){
  prediction <- predict(best_tree, row_value)
  return(prediction)
}

find_best_tree <- function(){
#//////оператор конкатенации строк///////
"%+%" <- function(...){
  paste0(...)
}

options(max.print=1000000)
dataset <- read.xlsx(getwd() %+% "/data/datas.xlsx", sheetIndex = 1)
dataset <- dataset[,c("fibroids_us_2",	"value_test2_bioch_DK",	"value_kd_ct",	"value_test3_bioch_MDA",	"value_test6_bioch_COD",	"value_test8_bioch_GSH",	"value_test11_bioch_vitE",	"value_test10_bioch_vitA", "age_visit1", "ethnicity", "value_prl_2")]

names(dataset)[1] <- "fibroids_us"
names(dataset)[2] <- "DK"
names(dataset)[3] <- "KD_CT"
names(dataset)[4] <- "MDA"
names(dataset)[5] <- "COD"
names(dataset)[6] <- "GSH"
names(dataset)[7] <- "vitE"
names(dataset)[8] <- "vitA"
names(dataset)[9] <- "age_visit1"
names(dataset)[10] <- "ethnicity"
names(dataset)[11] <- "PRL2"


#dataset <- subset(dataset, ethnicity=='1' |  ethnicity=='2')
dataset <- dataset %>%
  mutate(fibroids_us  = factor(fibroids_us, levels = c(0, 1), labels = c('No', 'Yes')),
         ethnicity = factor(ethnicity, levels = c(1, 2, 3), labels = c('White', 'Asian', 'Mixed')))

defineAge <- FALSE
dataset <- dataset[ -1126,]
dataset <- dataset[!is.na(dataset$fibroids_us), ]
dataset.tmp1 <- dataset[ ,c("fibroids_us",	"DK",	"KD_CT",	"MDA", "COD",	"GSH",	"vitE",	"vitA")]
dataset.tmp2 <- dataset[ ,c("fibroids_us",	"DK",	"KD_CT",	"MDA", "COD",	"GSH",	"vitE",	"vitA", "age_visit1")]
dataset.tmp3 <- dataset[ ,c("fibroids_us",	"DK",	"KD_CT",	"MDA", "COD",	"GSH",	"vitE",	"vitA", "age_visit1", "ethnicity")]
dataset.tmp4 <- dataset[ ,c("fibroids_us",	"DK",	"KD_CT",	"MDA", "COD",	"GSH",	"vitE",	"vitA", "age_visit1", "ethnicity", "PRL2")]

num_tmp <- 4
dataset <- switch(
  num_tmp,
  dataset.tmp1,
  dataset.tmp2,
  dataset.tmp3,
  dataset.tmp4
)

# if ("age_visit1" %in% names(dataset)){
#   defineAge <- TRUE
#   minAge <- 39
#   maxAge <- 48
#   dataset <- subset(dataset, age_visit1 > minAge & age_visit1 <= maxAge)
# }

shuffle_index <- sample(1:nrow(dataset)) #мешаем данные
dataset <- dataset[shuffle_index, ]

md.pattern(dataset) #package VIM, mice
aggr_plot <- aggr(dataset, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(dataset), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
imputed_data <- mice(dataset, m=5, method = 'pmm', seed = 100)
dataset2 <- complete(imputed_data,5)

define_grids(dataset2)

#ФУНКЦИИ ДЛЯ ТРЕНИРОВКИ МОДЕЛЕЙ

set.seed(1)
trainIndex <- createDataPartition(dataset2$fibroids_us, p = .8, 
                                  list = FALSE, 
                                  times = 1)

training <- dataset2[ trainIndex,]
testing <- dataset2[-trainIndex,]

print(dataset2[1,])

#Проверить соотношение процентов в обоих выборках (в тренировочной и в тестовой)
prop.table(table(training$fibroids_us))
prop.table(table(testing$fibroids_us)) 

list_trees <- list()
AUC_scores <- c()
AUC_scores
weights <- NULL
seeds <- seq(1000,1000+nrow(gridParameters_ctree), 1)

# tree <- party::ctree(fibroids_us ~., dataset2, controls = control_ctree, weights = weights)
# plot(tree)
# prediction <- predict(tree, testing)
# 
# # Calculate the overall accuracy.
# correct_ctree <- prediction == testdata$fibroids_us
#print(paste("% правильных прогнозов", mean(correct_ctree)))


for (i in 1:nrow(gridParameters_ctree)) {
  define_controlTree(gridParameters_ctree[i,])
  build_ctree(training, weights, seeds[i])
  list_trees[[i]] <- tree
  #out_info(i, tree)
  #plot_ctree(list_trees[[i]])
  AUC_scores <- c(AUC_scores, score_model(testdata = testing, tree = tree))
}

best_tree <<-  list_trees[[which.max(AUC_scores)]]
}