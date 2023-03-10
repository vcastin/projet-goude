rm(list=objects())

#### Élaboration des données prêtes à l'emploi

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

d <- readRDS("DONNEES/Data_RTE_janv2012_oc2022.RDS") # données brutes

d_ent <- filter(d, Year<=2018) # on entraîne entre 2012 et 2018
d_ent <- filter(d_ent, BH == 0) # on enlève les jours fériés

# on distingue les jours ouvrés des week-ends
d_ent_ouvre <- filter(d_ent, WeekDays != "Saturday" & WeekDays != "Sunday")
d_ent_we <- filter(d_ent, WeekDays =="Saturday" | WeekDays == "Sunday")

# construction d'un data frame par heure
H <- 24

for(i in c(1:H))
{
  assign(paste("d_ent_ouvre", i, sep="_"),filter(d_ent_ouvre, tod==i)) # d_ent_ouvre_i
}

for(i in c(1:H))
{
  assign(paste("d_ent_we", i, sep="_"),filter(d_ent_we, tod==i)) # d_ent_we_i
}

rm(d)

save(list = ls(all = TRUE), file= "DONNEES/data.rda")
