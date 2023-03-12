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

gbm_0 = gbm(equation, distribution = "gaussian", data = d_ent_ouvre_0, n.trees = Ntree, interaction.depth = 10,
         n.minobsinnode = 5, shrinkage = 0.05, bag.fraction = 0.5, train.fraction = 1,
         keep.data = FALSE, n.cores = 4)

pred_gbm_0 <- predict(gbm_0, n.trees = Ntree, single.tree=FALSE, newdata = d_test_ouvre_0)

mape(pred_gbm_0, d_test_ouvre_0$Load)

####### avec xgboost

d_xgb_0 <- data.matrix(d_ent_ouvre_0[,-c(1,2,3,4,5,6,7,8,20)])  # ici on prend bcp plus de covariables
d_xgb_1 <- data.matrix(d_test_ouvre_0[,-c(1,2,3,4,5,6,7,8,20)])

xgb_0 <- xgboost(params=list(subsample=0.9, eta = 0.05, max.depth = 10, colsample_bytree=1),
                data = d_xgb_0, label = d_ent_ouvre_0$Load,
                nthread = 4, objective = "reg:squarederror", nround = 1000,
                booster = "gbtree", verbose = 0)

pred_xgb <- predict(xgb_0, d_xgb_1)
mape(d_test_ouvre_0$Load, pred_xgb)

######## Sur tous les jeux de données

H <- 23

###### Avec gbm

Ntree <- 1000

equation <- as.formula("Load ~ Temp_s99+toy+Lundi+Mardi+Mercredi+Jeudi+Vendredi+Temp_s95+Load.48+Christmas_break+Summer_break+DLS+Temp+Temp_s95_min+Temp_s95_max+Temp_s99_min+Temp_s99_max")

for(i in c(0:H))
{
  assign(paste("gbm", i, sep="_"), gbm(equation, distribution = "gaussian", data=eval(parse(text=paste("d_ent_ouvre", i, sep="_"))),
                                       n.trees = Ntree, interaction.depth = 10, n.minobsinnode = 5, shrinkage = 0.05, bag.fraction = 0.5,
                                       train.fraction = 1, keep.data = FALSE, n.cores = 4))
}


for(i in c(0:H))
{
  assign(paste("pred_gbm", i, sep="_"), predict(eval(parse(text=paste("gbm", i, sep="_"))), n.trees = Ntree, single.tree=FALSE, newdata = eval(parse(text=paste("d_test_ouvre", i, sep="_")))))
}


# affichage des MAPE
mape_ouvre_RTE <- c()
for(i in c(0:H))
{
  mape_ouvre_RTE <- c(mape_ouvre_RTE, mape(eval(parse(text=paste("d_test_ouvre", i, sep="_")))$Load, eval(parse(text=paste("d_test_ouvre", i, sep="_")))$Forecast_RTE_intraday))
} # RTE pour comparaison

mape_ouvre_gbm <- c()
for(i in c(0:H))
{
  mape_ouvre_gbm <- c(mape_ouvre_gbm, mape(eval(parse(text=paste("d_test_ouvre", i, sep="_")))$Load, eval(parse(text=paste("pred_gbm", i, sep="_")))))
}


#### Avec xgboost

for(i in c(0:H))
{
  assign(paste("d_xgb_ent", i, sep="_"), data.matrix(eval(parse(text=paste("d_ent_ouvre", i, sep="_")))[,-c(1,2,3,4,5,6,7,8,20)]))
  assign(paste("d_xgb_test", i, sep="_"), data.matrix(eval(parse(text=paste("d_test_ouvre", i, sep="_")))[,-c(1,2,3,4,5,6,7,8,20)]))
  assign(paste("xgb", i, sep ="_"), xgboost(params=list(subsample=0.9, eta = 0.05, max.depth = 10, colsample_bytree=1),
                                            data = eval(parse(text=paste("d_xgb_ent", i, sep="_"))), label = eval(parse(text=paste("d_ent_ouvre", i, sep="_")))$Load,
                                            nthread = 4, objective = "reg:squarederror", nround = 1000,
                                            booster = "gbtree", verbose = 0))
}


for(i in c(0:H))
{
  assign(paste("pred_xgb", i, sep="_"), predict(eval(parse(text=paste("xgb", i, sep="_"))), eval(parse(text=paste("d_xgb_test", i, sep="_")))))
}


##### affichage des MAPE

mape_ouvre_xgb <- c()
for(i in c(0:H))
{
  mape_ouvre_xgb <- c(mape_ouvre_xgb, mape(eval(parse(text=paste("d_test_ouvre", i, sep="_")))$Load, eval(parse(text=paste("pred_xgb", i, sep="_")))))
}

# mape_ouvre

plot(mape_ouvre_gbm, main = "MAPE en fonction de l'heure, jours ouvrés (année 2019)", xlab = "Heure", ylab = "MAPE (%)", type="l", ylim= c(0,1.8), lab=c(5,4,0), col="blue")
lines(mape_ouvre_RTE, col="red")
lines(mape_ouvre_xgb, col="pink")
legend(x="bottomright", legend=c("gbm","RTE", "xgb"), col=c("blue","red", "pink"), pch=c(15,15,15))

save(list = ls(all = TRUE), file= "MODELES/boosting_arbres.rda")
