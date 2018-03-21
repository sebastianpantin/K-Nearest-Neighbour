# library list we will use
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(gplots)
library(gmodels)
library(lattice)
library(svdvis)
library(softImpute)
library(ElemStatLearn)
library(rpart.plot)
library(randomForest)
library(ranger)
library(rgl)
library(irlba)
library(bigmemory)
library(biglm)
library(biglars)
library(class)

# Data loading
prc <- read.csv("Prostate_Cancer.csv",stringsAsFactors = FALSE) 
str(prc)
prc <- prc[-1]
table(prc$diagnosis_result) # Malignant (M) or Benign (B), target variable
prc$diagnosis <- factor(prc$diagnosis_result, levels = c("B", "M"), labels = c("Benign", "Malignant"))
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) } #normalize to get the same scale
prc_n <- as.data.frame(lapply(prc[2:9], normalize))

# Divide into training and test sets
prc_train <- prc_n[1:65,]
prc_test <- prc_n[66:100,]
prc_train_labels <- prc[1:65, 1]
prc_test_labels <- prc[66:100,1]

# Use knn function to train and predict

prc_test_pred <- knn(train = prc_train, test = prc_test, cl = prc_train_labels, k = 10)

# Evaluate model performance
CrossTable(x = prc_test_labels, y = prc_test_pred, prop.chisq = FALSE)
