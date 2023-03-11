rm(list=objects())

library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(xts)
library(mgcv)
library(mgcViz)
library(gridExtra)
library(yarrr)
library(qgam)
library(magrittr)
library(rpart)
library(party)
library(tree)
library(rpart.plot)
library(progress)
library(plotmo)
library(caret)
library(randomForest)
library(ranger)
library(opera)
library(corrplot)
library(vip)
library(gbm)
library(xgboost)

set.seed(1)

load("DONNEES/data.rda")

# mesures d'erreur

rmse <- function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)), digits=0))
}

mape <- function(y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)), digits=2))
}

######### sur un seul jeu de données pour commencer

####### avec gbm

Ntree = 1000

equation <- as.formula("Load ~ Temp_s99+toy+Lundi+Mardi+Mercredi+Jeudi+Vendredi+Temp_s95+Load.48+Christmas_break+Summer_break+DLS+Temp+Temp_s95_min+Temp_s95_max+Temp_s99_min+Temp_s99_max")

gbm_0 = gbm(equation, distribution = "gaussian", data = d_ent_ouvre_1, n.trees = Ntree, interaction.depth = 10,
         n.minobsinnode = 5, shrinkage = 0.05, bag.fraction = 0.5, train.fraction = 1,
         keep.data = FALSE, n.cores = 4)

# best.iter <- gbm.perf(gbm_0, method="OOB", plot.it = TRUE, oobag.curve = TRUE)    
# best.iter  # en pratique on obtient environ 120 << 1000, mais 1000 marche mieux sur les données test
# which.min(-cumsum(gbm_0$oobag.improve))

pred_gbm_0 <- predict(gbm_0, n.trees = Ntree, single.tree=FALSE, newdata = d_test_ouvre_1)

mape(pred_gbm_0, d_test_ouvre_1$Load)

####### avec xgboost

d_xgb_0 <- data.matrix(d_ent_ouvre_1[,-c(1,2,3,4,5,6,7,8,20)])
d_xgb_1 <- data.matrix(d_test_ouvre_1[,-c(1,2,3,4,5,6,7,8,20)])

xgb_0 <- xgboost(params=list(subsample=0.9, eta = 0.05, max.depth = 10, colsample_bytree=1),
                data = d_xgb_0, label = d_ent_ouvre_1$Load,
                nthread = 4, objective = "reg:squarederror", nround = 1000,
                booster = "gbtree", verbose = 0)

pred_xgb <- predict(xgb_0, d_xgb_1)
mape(d_test_ouvre_1$Load, pred_xgb)




