#### Ce document regroupe les GAM que nous avons entraînés, et les sauvegarde pour pouvoir les charger 
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
## GAM simples
################

################## Cubic regression
equation <- Load~s(Load.48, k=3, bs="cr")+s(Temp_s95, k=5, bs="cr")+s(Temp_s99, k=5, bs="cr")+s(Temp, k=5, bs="cr")+s(toy, k=35, bs="cr")+te(Load.48,Temp_s99)+WeekDays+DLS+Christmas_break+Summer_break+s(Temp_s99_min, k=5)+s(Temp_s99_max, k=5)+s(Temp_s95_min, k=5)+s(Temp_s95_max, k=5)

for(i in c(0:H))
{
  assign(paste("gam_ouvre_cr", i, sep="_"), gam(equation, data=eval(parse(text=paste("d_ent_ouvre", i, sep="_")))))
}

# calcul des prédictions
for(i in c(0:H))
{
  assign(paste("pred_gam_ouvre", i, sep="_"), predict(eval(parse(text=paste("gam_ouvre_cr", i, sep="_"))), newdata = eval(parse(text=paste("d_test_ouvre", i, sep="_")))))
}

save(list = ls(all = TRUE), file= "MODELES/GAM.rda")
