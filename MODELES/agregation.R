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

load("DONNEES/data.rda")

# on importe tous les experts  # nom des experts -> prédictions
load("MODELES/GAM.rda") # gam_ouvre_cr_i -> pred_gam_ouvre_i
load("MODELES/forets_ouvre.rda") # foret_ouvre_i -> pred_foret_ouvre_i
load("MODELES/bagging_gam_tous.rda") # gam_bagg_i -> pred_gam_baggi
load("MODELES/boosting_arbres.rda") # xgb_i -> pred_xgb_i et pred_gbm_i
load("MODELES/stacking_forets.rda") # stack_ouvre_i -> pred_stack_ouvre_i
load("MODELES/stacking_boosting.rda") # gbm_stack_i -> pred_gbm_stack_i et pred_xgb_stack
load("MODELES/qgam_tous.rda") # Viz_i


rmse <- function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)), digits=0))
}

mape <- function(y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)), digits=2))
}


# #### sur d_ent_ouvre_0
# 
# pred_qgam_0 <- sapply(Viz_0, predict, newdata = d_test_ouvre_0)
# 
# experts <- cbind(pred_gam_ouvre_0, pred_foret_ouvre_0$predictions, pred_gbm_0,
#                  pred_xgb_0, pred_stack_ouvre_0$predictions, pred_gbm_stack_0,
#                  pred_xgb_stack_0, pred_qgam_0[, "0.4"], pred_qgam_0[, "0.45"], pred_qgam_0[, "0.5"], pred_qgam_0[, "0.55"], pred_gam_bagg0)
# 
# colnames(experts) <- c("gam", "foret", "gbm", "xgb", "stack_foret", "stack_gbm", "stack_xgb", "qgam0.4", "qgam0.45", "qgam0.5", "qgam0.55", "gam_bagg0_1", "gam_bagg0_2", "gam_bagg0_3", "gam_bagg0_4", "gam_bagg0_5", "gam_bagg0_6", "gam_bagg0_7", "gam_bagg0_8", "gam_bagg0_9", "gam_bagg0_10")
# 
# 
# oracle(Y=d_test_ouvre_0$Load, experts, model = "convex", loss.type = "square")
# 
# # algorithme EWA
# agg.online <- mixture(Y = d_test_ouvre_0$Load , experts = experts, model = 'EWA', 
#                      loss.type = "square", loss.gradient = T)
# # devrait converger vers le meilleur expert ?
# # ne le fait pas car la loi des données varie ?
# summary(agg.online)
# plot(agg.online, pause=F)
# 
# # algorithme MLpol
# agg.online<- mixture(Y = d_test_ouvre_1$Load , experts = experts, model = 'MLpol', 
#                      loss.type = "square", loss.gradient = T)
# summary(agg.online)
# plot(agg.online, pause=F)


# faire du bagging de qgam 0.45 ?
# comprendre pourquoi les résidus dévient aussi violemment vers le bas



######## sur tous les jeux de données

for(i in c(0:H))
{
  print(i)
  assign(paste("pred_qgam", i, sep="_"), sapply(eval(parse(text=paste("Viz", i, sep="_"))), predict, newdata = eval(parse(text=paste("d_test_ouvre", i, sep="_")))))
  assign(paste("experts", i, sep="_"), cbind(eval(parse(text=paste("pred_gam_ouvre", i, sep="_"))), eval(parse(text=paste("pred_foret_ouvre", i, sep="_")))$predictions, eval(parse(text=paste("pred_gbm", i, sep="_"))),
                                             eval(parse(text=paste("pred_xgb", i, sep="_"))), eval(parse(text=paste("pred_stack_ouvre", i, sep="_")))$predictions, eval(parse(text=paste("pred_gbm_stack", i, sep="_"))),
                                             eval(parse(text=paste("pred_xgb_stack", i, sep="_"))), eval(parse(text=paste("pred_qgam", i, sep="_")))[, "0.4"], eval(parse(text=paste("pred_qgam", i, sep="_")))[, "0.45"], eval(parse(text=paste("pred_qgam", i, sep="_")))[, "0.5"], eval(parse(text=paste("pred_qgam", i, sep="_")))[, "0.55"], eval(parse(text=paste("pred_gam_bagg", i, sep="")))))
  eval(parse(text=paste(paste("colnames(experts", i, sep="_"), ") <- c(\"gam\", \"foret\", \"gbm\", \"xgb\", \"stack_foret\", \"stack_gbm\", \"stack_xgb\", \"qgam0.4\", \"qgam0.45\", \"qgam0.5\", \"qgam0.55\", \"gam_bagg0_1\", \"gam_bagg0_2\", \"gam_bagg0_3\", \"gam_bagg0_4\", \"gam_bagg0_5\")" )))
  # eval(parse(text=paste("oracle(Y=", paste(paste("d_test_ouvre", i, sep="_"), "$Load,"), paste("experts", i, sep="_"), ", model = \" convex \", loss.type = \" square \")"   )))
  eval(parse(text=paste(paste("agg.online", i, sep="_"), "<- mixture(Y=", paste(paste("d_test_ouvre", i, sep="_"), "$Load", sep=""), ", experts =", paste("experts", i, sep="_"), ", model = \"MLpol\", loss.type = \"square\", loss.gradient = T)"   )))
}

mape_agg <- c(mape(agg.online_0$prediction, d_test_ouvre_0$Load),
              mape(agg.online_1$prediction, d_test_ouvre_1$Load),
              mape(agg.online_2$prediction, d_test_ouvre_2$Load),
              mape(agg.online_3$prediction, d_test_ouvre_3$Load),
              mape(agg.online_4$prediction, d_test_ouvre_4$Load),
              mape(agg.online_5$prediction, d_test_ouvre_5$Load),
              mape(agg.online_6$prediction, d_test_ouvre_6$Load),
              mape(agg.online_7$prediction, d_test_ouvre_7$Load),
              mape(agg.online_8$prediction, d_test_ouvre_8$Load),
              mape(agg.online_9$prediction, d_test_ouvre_9$Load),
              mape(agg.online_10$prediction, d_test_ouvre_10$Load),
              mape(agg.online_11$prediction, d_test_ouvre_11$Load),
              mape(agg.online_12$prediction, d_test_ouvre_12$Load),
              mape(agg.online_13$prediction, d_test_ouvre_13$Load),
              mape(agg.online_14$prediction, d_test_ouvre_14$Load),
              mape(agg.online_15$prediction, d_test_ouvre_15$Load),
              mape(agg.online_16$prediction, d_test_ouvre_16$Load),
              mape(agg.online_17$prediction, d_test_ouvre_17$Load),
              mape(agg.online_18$prediction, d_test_ouvre_18$Load),
              mape(agg.online_19$prediction, d_test_ouvre_19$Load),
              mape(agg.online_20$prediction, d_test_ouvre_20$Load),
              mape(agg.online_21$prediction, d_test_ouvre_21$Load),
              mape(agg.online_22$prediction, d_test_ouvre_22$Load),
              mape(agg.online_23$prediction, d_test_ouvre_23$Load))

plot(mape_agg, main = "MAPE en fonction de l'heure, jours ouvrés (année 2019)", xlab = "Heure", ylab = "MAPE (%)", type="l", ylim= c(0,1.8), lab=c(5,4,0), col="blue")
lines(mape_ouvre_RTE, col="red")

legend(x="bottomright", legend=c("agregation","RTE"), col=c("blue","red"), pch=c(15,15))

save(mape_agg, agg.online_0, agg.online_1, agg.online_2,agg.online_3,agg.online_4,agg.online_5,agg.online_6,agg.online_7,agg.online_8,agg.online_9,agg.online_10,agg.online_11,agg.online_12,agg.online_13,agg.online_14,agg.online_15,agg.online_16,agg.online_17,agg.online_18,agg.online_19,agg.online_20,agg.online_21,agg.online_22,agg.online_23, file= "MODELES/agregation.rda")




summary(agg.online)
plot(agg.online, pause=F)




