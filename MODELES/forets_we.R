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

d <- readRDS("Data_RTE_janv2012_oc2022.RDS")

d_ent <- filter(d, Year<=2018) # on entraîne entre 2012 et 2018

# on enlève les jours fériés
d_ent <- filter(d_ent, BH == 0)

# on distingue les jours ouvrés des week-ends
d_ent_we <- filter(d_ent, WeekDays =="Saturday" | WeekDays == "Sunday")

# construction d'un data frame par heure
H <- 24

for(i in c(1:H))
{
  assign(paste("d_ent_we", i, sep="_"),filter(d_ent_we, tod==i)) # d_ent_we_i
}

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
## Forêts aléatoires simples, week-end
################

formule_1 <- "Load ~ Temp_s99+toy+WeekDays+Temp_s95+Load.48+Christmas_break+Summer_break+DLS+Temp"

# calcul des modèles
for(i in c(1:H))
{
  assign(paste("foret_we", i, sep="_"), ranger(formule_1, data=eval(parse(text=paste("d_ent_we", i, sep="_"))), importance = "permutation", num.trees = 150, mtry=8))
}






save(list = ls(all = TRUE), file= "forets_we.rda")






