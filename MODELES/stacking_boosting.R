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
library(gbm)
library(xgboost)

set.seed(1)

load("MODELES/GAM.rda")
load("DONNEES/data.rda")

rmse <- function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)), digits=0))
}

mape <- function(y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)), digits=2))
}

# nouvelles bases de données test avec les splines du gam
for(i in c(0:H))
{
  assign(paste("pred_ouvre", i, sep="_"), predict(eval(parse(text=paste("gam_ouvre_cr", i, sep="_"))), newdata = eval(parse(text=paste("d_test_ouvre", i, sep="_"))), type='terms'))
  eval(parse(text=paste("colnames(pred_ouvre_", i, ")<-", "paste0(\"gterms_\", c(1:ncol(eval(parse(text=paste(\"pred_ouvre\", i, sep=\"_\"))))))", sep="" )))
  assign(paste("d_test_ouvre_bis", i, sep="_"), eval(parse(text=paste("data.frame(d_test_ouvre_", i, ",pred_ouvre_", i, ")", sep="") )))
}

# nouvelles bases de données d'entraînement avec les splines du gam
for(i in c(0:H))
{
  assign(paste("fit_ouvre", i, sep="_"), predict(eval(parse(text=paste("gam_ouvre_cr", i, sep="_"))), newdata = eval(parse(text=paste("d_ent_ouvre", i, sep="_"))), type='terms'))
  eval(parse(text=paste("colnames(fit_ouvre_", i, ")<-", "paste0(\"gterms_\", c(1:ncol(eval(parse(text=paste(\"fit_ouvre\", i, sep=\"_\"))))))", sep="" )))
  assign(paste("d_ent_ouvre_bis", i, sep="_"), eval(parse(text=paste("data.frame(d_ent_ouvre_", i, ", fit_ouvre_", i, ")", sep="") )))
}

g_terms <- paste0("gterms_", c(1:ncol(eval(parse(text=paste("pred_ouvre", i, sep="_"))))))
formule_2 <- paste0("Load ~Temp_s99+toy+WeekDays+Temp_s95+Load.48+Christmas_break+Summer_break+DLS+Temp+Temp_s95_min+Temp_s95_max+Temp_s99_min+Temp_s99_max+",  paste0(g_terms, collapse=' + '))


# calcul des modèles

H <- 23

###### Avec gbm

Ntree <- 1000

equation <- as.formula("Load ~ Temp_s99+toy+Lundi+Mardi+Mercredi+Jeudi+Vendredi+Temp_s95+Load.48+Christmas_break+Summer_break+DLS+Temp+Temp_s95_min+Temp_s95_max+Temp_s99_min+Temp_s99_max")

for(i in c(0:H))
{
  assign(paste("gbm_stack", i, sep="_"), gbm(equation, distribution = "gaussian", data=eval(parse(text=paste("d_ent_ouvre_bis", i, sep="_"))),
                                       n.trees = Ntree, interaction.depth = 10, n.minobsinnode = 5, shrinkage = 0.05, bag.fraction = 0.5,
                                       train.fraction = 1, keep.data = FALSE, n.cores = 4))
}


for(i in c(0:H))
{
  assign(paste("pred_gbm_stack", i, sep="_"), predict(eval(parse(text=paste("gbm_stack", i, sep="_"))), n.trees = Ntree, single.tree=FALSE, newdata = eval(parse(text=paste("d_test_ouvre_bis", i, sep="_")))))
}


# calcul des MAPE
mape_ouvre_RTE <- c()
for(i in c(0:H))
{
  mape_ouvre_RTE <- c(mape_ouvre_RTE, mape(eval(parse(text=paste("d_test_ouvre", i, sep="_")))$Load, eval(parse(text=paste("d_test_ouvre", i, sep="_")))$Forecast_RTE_intraday))
} # RTE pour comparaison

mape_ouvre_sgbm <- c()
for(i in c(0:H))
{
  mape_ouvre_sgbm <- c(mape_ouvre_sgbm, mape(eval(parse(text=paste("d_test_ouvre_bis", i, sep="_")))$Load, eval(parse(text=paste("pred_gbm_stack", i, sep="_")))))
}


#### Avec xgboost

for(i in c(0:H))
{
  print(i)
  assign(paste("d_xgb_stack_ent", i, sep="_"), data.matrix(eval(parse(text=paste("d_ent_ouvre_bis", i, sep="_")))[,-c(1,2,3,4,5,6,7,8,20)]))
  assign(paste("d_xgb_stack_test", i, sep="_"), data.matrix(eval(parse(text=paste("d_test_ouvre_bis", i, sep="_")))[,-c(1,2,3,4,5,6,7,8,20)]))
  assign(paste("xgb_stack", i, sep ="_"), xgboost(params=list(subsample=0.9, eta = 0.05, max.depth = 10, colsample_bytree=1),
                                            data = eval(parse(text=paste("d_xgb_stack_ent", i, sep="_"))), label = eval(parse(text=paste("d_ent_ouvre_bis", i, sep="_")))$Load,
                                            nthread = 4, objective = "reg:squarederror", nround = 1000,
                                            booster = "gbtree", verbose = 0))
}


for(i in c(0:H))
{
  assign(paste("pred_xgb_stack", i, sep="_"), predict(eval(parse(text=paste("xgb_stack", i, sep="_"))), eval(parse(text=paste("d_xgb_stack_test", i, sep="_")))))
}


##### affichage des MAPE

mape_ouvre_sxgb <- c()
for(i in c(0:H))
{
  mape_ouvre_sxgb <- c(mape_ouvre_sxgb, mape(eval(parse(text=paste("d_test_ouvre_bis", i, sep="_")))$Load, eval(parse(text=paste("pred_xgb_stack", i, sep="_")))))
}

# mape_ouvre

plot(mape_ouvre_sgbm, main = "MAPE en fonction de l'heure, jours ouvrés (année 2019)", xlab = "Heure", ylab = "MAPE (%)", type="l", ylim= c(0,1.8), lab=c(5,4,0), col="blue")
lines(mape_ouvre_RTE, col="red")
lines(mape_ouvre_sxgb, col="pink")
legend(x="bottomright", legend=c("gbm_stack","RTE", "xgb_stack"), col=c("blue","red", "pink"), pch=c(15,15,15))













rm(pred_ouvre_0)
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

rm(fit_ouvre_0)
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

rm(gam_ouvre_cr_0)
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


save(list = ls(all = TRUE), file= "MODELES/stacking_boosting.rda")

