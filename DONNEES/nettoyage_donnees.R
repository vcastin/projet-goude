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

# on encode WeekDays (jours ouvrés) par des indicatrices

Lundi <- as.double(d$WeekDays == "Monday")
Mardi <- as.double(d$WeekDays == "Tuesday")
Mercredi <- as.double(d$WeekDays == "Wednesday")
Jeudi <- as.double(d$WeekDays == "Thursday")
Vendredi <- as.double(d$WeekDays == "Friday")

d <- data.frame(d, Lundi, Mardi, Mercredi, Jeudi, Vendredi)

d <- filter(d, BH == 0) # on enlève les jours fériés

d_ent <- filter(d, Year<=2018) # on entraîne entre 2012 et 2018
d_test <- filter(d, Year==2019) # on teste sur 2019

# on distingue les jours ouvrés des week-ends
d_ent_ouvre <- filter(d_ent, WeekDays != "Saturday" & WeekDays != "Sunday")
d_test_ouvre <- filter(d_test, WeekDays != "Saturday" & WeekDays != "Sunday")

# construction d'un data frame par heure
H <- 23

for(i in c(0:H))
{
  assign(paste("d_ent_ouvre", i, sep="_"),filter(d_ent_ouvre, tod==2*i)) # d_ent_ouvre_i
}

for(i in c(0:H))
{
  assign(paste("d_test_ouvre", i, sep="_"),filter(d_test_ouvre, tod==2*i))
}

rm(d)

save(list = ls(all = TRUE), file= "DONNEES/data.rda")

