#### Ce document regroupe les modèles de forêts que nous avons entraînés, et les sauvegarde pour pouvoir les charger 
#### directement dans le R markdown du rapport.

rm(list=objects())

################
## Importations préliminaires
################

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

################
## Forêts aléatoires simples, jours ouvrés
################

formule_1 <- "Load ~ Temp_s99+toy+WeekDays+Temp_s95+Load.48+Christmas_break+Summer_break+DLS+Temp+Temp_s95_min+Temp_s95_max+Temp_s99_min+Temp_s99_max"

# calcul des modèles
for(i in c(0:H))
{
  assign(paste("foret_ouvre", i, sep="_"), ranger(formule_1, data=eval(parse(text=paste("d_ent_ouvre", i, sep="_"))), importance = "permutation", num.trees = 150, mtry=8))
}

# calcul des prédictions
for(i in c(0:H))
{
  assign(paste("pred_foret_ouvre", i, sep="_"), predict(eval(parse(text=paste("foret_ouvre", i, sep="_"))), data = eval(parse(text=paste("d_test_ouvre", i, sep="_")))))
}

save(list = ls(all = TRUE), file= "MODELES/forets_ouvre.rda")






