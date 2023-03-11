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

# bootstrap et entraînement de différents GAM

equation <- Load~s(Load.48,k=3, bs="cr")+s(Temp_s95, k=5, bs="cr")+s(Temp_s99, k=5, bs="cr")+s(Temp, k=5, bs="cr")+s(toy, k=35, bs="cr")+te(Load.48,Temp_s99)+WeekDays+DLS+Christmas_break+Summer_break+s(Temp_s99_min, k=5)+s(Temp_s99_max, k=5)+s(Temp_s95_min, k=5)+s(Temp_s95_max, k=5)


bagging <- function(Nbag, data_app, equation, size=n)
{
  n <- nrow(data_app)
  bagg <- list()
  for(i in c(1:Nbag))
  {
    s <- sample(c(1:n), size, replace=TRUE)  # size détermine la taille du nouveau jeu de données
    data.bagg <- data_app[s,]
    bagg[[i]] <- gam(equation, data=data.bagg)
  }
  return(bagg)
}


#### 10 nouveaux GAM

Nbag <-10

n <- nrow(d_ent_ouvre_1)

gam.bagg <- bagging(Nbag, data_app=d_ent_ouvre_1, equation, size=floor(0.8*n))

save(gam.bagg, file="MODELES/bagging_gam.rda")

