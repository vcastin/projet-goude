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

# choix de mtry par validation croisée

grid_mtry <-  expand.grid(mtry = seq(1,15,by=1), 
                          min.node.size = 5,
                          splitrule = "variance")

fitControl <- trainControl(method="cv", number=16,
                           verboseIter = TRUE)

fit = caret::train(
  x = d_ent_ouvre_bis_1,
  y = d_ent_ouvre_bis_1$Load,
  method = 'ranger',
  num.trees = 100,
  tuneGrid = grid_mtry,
  trControl = fitControl)

#print(fit)

p <- plot(fit, main = "Choix du paramètre mtry par validation croisée \n Jeu de données : d_ent_ouvre_1", xlab = "mtry", ylab="RMSE")
print(p)

# mtry = 10 a l'air pas mal

# calcul des modèles
for(i in c(1:H))
{
  assign(paste("stack_ouvre", i, sep="_"), ranger(formule_2, data=eval(parse(text=paste("d_ent_ouvre_bis", i, sep="_"))), importance = "permutation", num.trees = 150, mtry=10))
}

# calcul des prédictions
for(i in c(1:H))
{
  assign(paste("pred_stack_ouvre", i, sep="_"), predict(eval(parse(text=paste("stack_ouvre", i, sep="_"))), data = eval(parse(text=paste("d_test_ouvre_bis", i, sep="_")))))
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

save(list = ls(all = TRUE), file= "MODELES/stacking_forets.rda")

