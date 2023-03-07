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
d_test <- filter(d, Year==2019) # on teste sur 2019

# on enlève les jours fériés
d_ent <- filter(d_ent, BH == 0)
d_test <- filter(d_test, BH == 0)

# on distingue les jours ouvrés des week-ends
d_ent_ouvre <- filter(d_ent, WeekDays != "Saturday" & WeekDays != "Sunday")
d_test_ouvre <- filter(d_test, WeekDays != "Saturday" & WeekDays != "Sunday")

d_ent_we <- filter(d_ent, WeekDays =="Saturday" | WeekDays == "Sunday")
d_test_we <- filter(d_test, WeekDays =="Saturday" | WeekDays == "Sunday")

H <- 24

for(i in c(1:H))
{
  assign(paste("d_ent_ouvre", i, sep="_"),filter(d_ent_ouvre, tod==i)) # d_ent_ouvre_i
}

for(i in c(1:H))
{
  assign(paste("d_ent_we", i, sep="_"),filter(d_ent_we, tod==i)) # d_ent_we_i
}

for(i in c(1:H))
{
  assign(paste("d_test_ouvre", i, sep="_"),filter(d_test_ouvre, tod==i)) # d_test_ouvre_i
}

for(i in c(1:H))
{
  assign(paste("d_test_we", i, sep="_"),filter(d_test_we, tod==i)) # d_test_we_i
}

load("MODELES/GAM.rda")

# nouvelles bases de données test avec les splines du gam
for(i in c(1:H))
{
  assign(paste("pred_ouvre", i, sep="_"), predict(eval(parse(text=paste("gam_ouvre_cr", i, sep="_"))), newdata = eval(parse(text=paste("d_test_ouvre", i, sep="_"))), type='terms'))
  eval(parse(text=paste("colnames(pred_ouvre_", i, ")<-", "paste0(\"gterms_\", c(1:ncol(eval(parse(text=paste(\"pred_ouvre\", i, sep=\"_\"))))))", sep="" )))
  assign(paste("d_test_ouvre_bis", i, sep="_"), eval(parse(text=paste("data.frame(d_test_ouvre_", i, ",pred_ouvre_", i, ")", sep="") )))
}

# nouvelles bases de données d'entraînement avec les splines du gam
for(i in c(1:H))
{
  assign(paste("fit_ouvre", i, sep="_"), predict(eval(parse(text=paste("gam_ouvre_cr", i, sep="_"))), newdata = eval(parse(text=paste("d_ent_ouvre", i, sep="_"))), type='terms'))
  eval(parse(text=paste("colnames(fit_ouvre_", i, ")<-", "paste0(\"gterms_\", c(1:ncol(eval(parse(text=paste(\"fit_ouvre\", i, sep=\"_\"))))))", sep="" )))
  assign(paste("d_ent_ouvre_bis", i, sep="_"), eval(parse(text=paste("data.frame(d_ent_ouvre_", i, ", fit_ouvre_", i, ")", sep="") )))
}

g_terms <- paste0("gterms_", c(1:ncol(eval(parse(text=paste("pred_ouvre", i, sep="_"))))))
formule_2 <- paste0("Load ~Temp_s99+toy+WeekDays+Temp_s95+Load.48+Christmas_break+Summer_break+DLS+Temp+Temp_s95_min+Temp_s95_max+Temp_s99_min+Temp_s99_max+",  paste0(g_terms, collapse=' + '))

# calcul des modèles
for(i in c(1:H))
{
  assign(paste("stack_ouvre", i, sep="_"), ranger(formule_2, data=eval(parse(text=paste("d_ent_ouvre_bis", i, sep="_"))), importance = "permutation", num.trees = 150, mtry=8))
}

rm(pred_ouvre_1)
rm(pred_ouvre_2)
rm(pred_ouvre_3)
rm(pred_ouvre_4)
rm(pred_ouvre_5)
rm(pred_ouvre_6)
rm(pred_ouvre_7)
rm(pred_ouvre_8)
rm(pred_ouvre_9)
rm(pred_ouvre_10)
rm(pred_ouvre_11)
rm(pred_ouvre_12)
rm(pred_ouvre_13)
rm(pred_ouvre_14)
rm(pred_ouvre_15)
rm(pred_ouvre_16)
rm(pred_ouvre_17)
rm(pred_ouvre_18)
rm(pred_ouvre_19)
rm(pred_ouvre_20)
rm(pred_ouvre_21)
rm(pred_ouvre_22)
rm(pred_ouvre_23)
rm(pred_ouvre_24)

rm(fit_ouvre_1)
rm(fit_ouvre_2)
rm(fit_ouvre_3)
rm(fit_ouvre_4)
rm(fit_ouvre_5)
rm(fit_ouvre_6)
rm(fit_ouvre_7)
rm(fit_ouvre_8)
rm(fit_ouvre_9)
rm(fit_ouvre_10)
rm(fit_ouvre_11)
rm(fit_ouvre_12)
rm(fit_ouvre_13)
rm(fit_ouvre_14)
rm(fit_ouvre_15)
rm(fit_ouvre_16)
rm(fit_ouvre_17)
rm(fit_ouvre_18)
rm(fit_ouvre_19)
rm(fit_ouvre_20)
rm(fit_ouvre_21)
rm(fit_ouvre_22)
rm(fit_ouvre_23)
rm(fit_ouvre_24)

rm(gam_ouvre_cr_1)
rm(gam_ouvre_cr_2)
rm(gam_ouvre_cr_3)
rm(gam_ouvre_cr_4)
rm(gam_ouvre_cr_5)
rm(gam_ouvre_cr_6)
rm(gam_ouvre_cr_7)
rm(gam_ouvre_cr_8)
rm(gam_ouvre_cr_9)
rm(gam_ouvre_cr_10)
rm(gam_ouvre_cr_11)
rm(gam_ouvre_cr_12)
rm(gam_ouvre_cr_13)
rm(gam_ouvre_cr_14)
rm(gam_ouvre_cr_15)
rm(gam_ouvre_cr_16)
rm(gam_ouvre_cr_17)
rm(gam_ouvre_cr_18)
rm(gam_ouvre_cr_19)
rm(gam_ouvre_cr_20)
rm(gam_ouvre_cr_21)
rm(gam_ouvre_cr_22)
rm(gam_ouvre_cr_23)
rm(gam_ouvre_cr_24)

save(list = ls(all = TRUE), file= "MODELES/stacking.rda")
