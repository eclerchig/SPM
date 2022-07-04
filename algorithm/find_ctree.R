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
library("stringr")
library("parallel")
library("doParallel")

#ФУНКЦИИ ДЛЯ ТРЕНИРОВКИ МОДЕЛЕЙ
define_grids <- function(dataframe){
  gridParameters_ctree <<- expand.grid(mtry = c(Inf,2,4,6),
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
  control_ctree <<- partykit::ctree_control(mincriterion = grid[1,'mincriterion'],
                                         mtry = grid[1,'mtry'],
                                         maxdepth = grid[1,'maxdepth'])
}

build_ctree <- function(data, weights, seed) {
  if (!is.null(seed)) set.seed(seed)
  tree <<- partykit::ctree(fibroids_us ~., data, control = control_ctree)
}

plot_ctree <- function(ctree){
  plot(ctree)
}

out_info <- function(n_tree, tree, seeds){
  cat("Номер дерева: ", n_tree,"\n")
  cat("seed: ", seeds[n_tree],"\n")
  cat("mtry: ", gridParameters_ctree[n_tree, 'mtry'],"\n")
  cat("maxdepth: ", gridParameters_ctree[n_tree, 'maxdepth'],"\n")
  cat("mincriterion: ", gridParameters_ctree[n_tree, 'mincriterion'],"\n")
}

ROC_plot <- function(tree, testdata, AUC) {
  prediction <- predict(tree, testdata)
  prob <- predict(tree, testdata, type = "prob")
  # Calculate the overall accuracy.
  correct_ctree <- prediction == testdata$fibroids_us
  # Extract the class probabilities.
  probabilities <- 1 - prob[,1]
  pred <- prediction(probabilities, testdata$fibroids_us)
  perf <- performance(pred,"tpr","fpr")
  par(mar=c(1,1,1,1))
  print("check")
  x <- unlist(perf@x.values)
  y <- unlist(perf@y.values)
  plot <- ggplot(data = data.frame(x = x,y = y)) +
                 geom_line(mapping = aes(x = x, y = y)) + 
                 geom_abline(intercept = 0, slope = 1) +
                 labs(x = perf@x.name %+% "(Специфичность)", y = perf@y.name %+% "(Чувствительность)", title = "AUC = " %+% as.character(AUC))
  return (plot)
}

predict_fibroids <- function(row_value){
  prediction <- predict(best_tree[[1]]$plotTree, row_value)
  prob <- predict(best_tree[[1]]$plotTree, row_value, type = "prob")
  node <- predict(best_tree[[1]]$plotTree, row_value, type = "node")
  return(list("prediction" = prediction, "ctree" = best_tree[[1]]$plotTree, "prob" = prob, "node" = node))
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

dataset <- dataset[ -1126,]
dataset <- subset(dataset, ethnicity=='1' |  ethnicity=='2')
dataset <- dataset %>%
  mutate(fibroids_us  = factor(fibroids_us, levels = c(0, 1), labels = c('No', 'Yes')),
         ethnicity = factor(ethnicity, levels = c(1, 2), labels = c('White', 'Asian')))

defineAge <- FALSE
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

shuffle_index <- sample(1:nrow(dataset)) 
dataset <- dataset[shuffle_index, ]

md.pattern(dataset) #package VIM, mice
aggr_plot <- aggr(dataset, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(dataset), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
imputed_data <- mice(dataset, m=5, method = 'pmm', seed = 100)
dataset2 <<- complete(imputed_data,5)

define_grids(dataset2)

#ФУНКЦИИ ДЛЯ ТРЕНИРОВКИ МОДЕЛЕЙ

trainIndex <- createDataPartition(dataset2$fibroids_us, p = .8, 
                                  list = FALSE, 
                                  times = 1)

training <- dataset2[ trainIndex,]
testing <- dataset2[-trainIndex,]

seeds_tree <- seq(1000,1000+nrow(gridParameters_ctree), 1)


ResultCTREE <- function(plotTree=NULL, pred=NULL, scoreAUC=NULL, test_data=NULL, train_data = NULL, plotROC=NULL, control = NULL, seed = NULL, seed_bag = NULL)
{
  me <- list(
    plotTree = plotTree,
    pred = pred,
    scoreAUC = scoreAUC,
    test_data = test_data,
    train_data = train_data,
    plotROC = plotROC,
    control = control,
    seed = seed,
    seed_bag = seed_bag
  )
  
  ## Set the name for the class
  class(me) <- append(class(me),"ResultCTREE")
  return(me)
}

cl <- makeCluster(8) # use 8 workers
registerDoParallel(cl) # register the parallel backend

source("R_functions.R")

bestAUC <<- 0
num_best <<- 0
num_bags <<- 10
seeds_bags <- 1000
best_trees <- list()

for (j in 1:nrow(gridParameters_ctree)) {
  define_controlTree(gridParameters_ctree[j,])
  listAUC <- c()

  bagging <-
    foreach(i = 1:num_bags,
            .packages=c('partykit','ROCR','magrittr','ggplot2'),
            .export = c("control_ctree", "score_model", "ROC_plot")
    ) %dopar% {
      # bootstrap copy of training data
      set.seed(seeds_bags)
      index <- sample(nrow(training), replace = TRUE)
      training_boot <- training[index, ]

      # fit tree to bootstrap copy
      set.seed(seeds_tree[j])
      bagged_tree <- partykit::ctree(
        fibroids_us ~.,
        control = control_ctree,
        data = training_boot
      )
      result <- ResultCTREE()
      result$plotTree <- bagged_tree
      result$pred <- predict(bagged_tree, testing)
      result$scoreAUC <- score_model(bagged_tree, testing)
      result$test_data <- testing
      result$plotROC <- ROC_plot(bagged_tree, testing, result$scoreAUC)
      result$train_data <- training_boot
      result$control <- control_ctree
      result$seed <- seeds_tree[j]
      result$seed_bag <- seeds_bags 
      return(result)
    }
  
  for (i in 1:10){
    listAUC <- c(listAUC,bagging[[i]]$scoreAUC)
  }
  best_trees[[j]] <- ResultCTREE()
  best_trees[[j]]$plotTree <- bagging[[which.max(listAUC)]]$plotTree
  best_trees[[j]]$pred <- bagging[[which.max(listAUC)]]$pred
  best_trees[[j]]$scoreAUC <- bagging[[which.max(listAUC)]]$scoreAUC
  best_trees[[j]]$test_data <- bagging[[which.max(listAUC)]]$test_data
  best_trees[[j]]$plotROC <- bagging[[which.max(listAUC)]]$plotROC
  best_trees[[j]]$train_data <- bagging[[which.max(listAUC)]]$train_data
  best_trees[[j]]$control <- bagging[[which.max(listAUC)]]$control
  best_trees[[j]]$seed <- bagging[[which.max(listAUC)]]$seed
  best_trees[[j]]$seed_bag <- bagging[[which.max(listAUC)]]$seed_bag
  
  if (best_trees[[j]]$scoreAUC > bestAUC) {
    bestAUC <- best_trees[[j]]$scoreAUC
    num_best <<- j
  }
}

best_tree <- best_trees[[num_best]]
return(list(best_tree, best_trees))
}

