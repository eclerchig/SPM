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

#//////оператор конкатенации строк///////
"%+%" <- function(...){
  paste0(...)
}

options(max.print=1000000)
dataset <- read.xlsx(getwd() %+% "/data/datas.xlsx", sheetIndex = 1)
dataset
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

num_tmp <- 3
dataset <- switch(
  num_tmp,
  dataset.tmp1,
  dataset.tmp2,
  dataset.tmp3,
  dataset.tmp4
)

if ("age_visit1" %in% names(dataset)){
  defineAge <- TRUE
  minAge <- 39
  maxAge <- 48
  dataset <- subset(dataset, age_visit1 > minAge & age_visit1 <= maxAge)
}

summary(dataset)

shuffle_index <- sample(1:nrow(dataset)) #мешаем данные
dataset <- dataset[shuffle_index, ]

head(dataset)