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
load("MODELES/bagging_gam.rda") # gam_bagg_i[k], k=1,..,10 -> pred_gam_bagg1_k
load("MODELES/boosting_arbres.rda") # xgb_i -> pred_xgb_i et pred_gbm_i
load("MODELES/stacking_forets.rda") # stack_ouvre_i -> pred_stack_ouvre_i
load("MODELES/stacking_boosting.rda") # gbm_stack_i -> pred_gbm_stack_i et pred_xgb_stack
load("ajout-guillaume1/qgam_0.05...0.95") # Viz


rmse <- function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)), digits=0))
}

mape <- function(y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)), digits=2))
}

pred_qgam <- sapply(Viz, predict, newdata = d_test_ouvre_1)  ### Problème : on n'a pas les mêmes jeux de données, à uniformiser !

#bagg_list <- paste0("pred_gam_bagg1_", c(1:10), sep=", ")
experts <- cbind(pred_gam_ouvre_1, pred_foret_ouvre_1$predictions, pred_gam_bagg1_1, pred_gam_bagg1_2, pred_gam_bagg1_3, pred_gam_bagg1_4, pred_gam_bagg1_5, pred_gam_bagg1_6, pred_gam_bagg1_7, pred_gam_bagg1_8, pred_gam_bagg1_9, pred_gam_bagg1_10, 
                 pred_xgb_1$forecast, pred_stack_ouvre_1$predictions, pred_gbm_stack_1,
                 pred_xgb_stack_1, pred) # à modifier à partir de cette ligne

experts <- cbind(pred_foret_ouvre_1$predictions, pred_stack_ouvre_1$predictions, pred_gam_ouvre_tp_1, pred_gam_ouvre_cr_1, pred_qgam[, "0.4"], pred_qgam[, "0.5"], pred_qgam[, "0.6"], bagg.forecast)
colnames(experts) <- c("foret", "stack", "gam_tp", "gam_cr", "qgam0.4", "qgam0.5", "qgam0.6", "gam_bagg_1", "gam_bagg_2", "gam_bagg_3", "gam_bagg_4", "gam_bagg_5", "gam_bagg_6", "gam_bagg_7", "gam_bagg_8", "gam_bagg_9", "gam_bagg_10")


# experts <- cbind(pred_foret_ouvre_1$predictions, pred_stack_ouvre_1$predictions, pred_gam_ouvre_tp_1, pred_gam_ouvre_cr_1, pred_qgam[, "0.4"], pred_qgam[, "0.5"], pred_qgam[, "0.6"])
# colnames(experts) <- c("foret", "stack", "gam_tp", "gam_cr", "qgam0.4", "qgam0.5", "qgam0.6")

# experts <- cbind(pred_foret_ouvre_1$predictions, pred_stack_ouvre_1$predictions, pred_gam_ouvre_tp_1, pred_gam_ouvre_cr_1, pred_qgam[, "0.4"], pred_qgam[, "0.5"], pred_qgam[, "0.6"], d_test_ouvre_1$Forecast_RTE_dayahead)
# colnames(experts) <- c("foret", "stack", "gam_tp", "gam_cr", "qgam0.4", "qgam0.5", "qgam0.6", "pred_RTE")

experts <- cbind(pred_foret_ouvre_1$predictions, pred_stack_ouvre_1$predictions, pred_gam_ouvre_tp_1, pred_qgam[, "0.4"], pred_qgam[, "0.5"], pred_qgam[, "0.6"])
colnames(experts) <- c("foret", "stack", "gam_tp", "qgam0.4", "qgam0.5", "qgam0.6")

oracle(Y=d_test_ouvre_1$Load, experts, model = "convex", loss.type = "square")

# algorithme EWA
agg.online<- mixture(Y = d_test_ouvre_1$Load , experts = experts, model = 'EWA', 
                     loss.type = "square", loss.gradient = T)
# devrait converger vers le meilleur expert ?
# ne le fait pas car la loi des données varie ?
summary(agg.online)
plot(agg.online, pause=F)

# algorithme MLpol
agg.online<- mixture(Y = d_test_ouvre_1$Load , experts = experts, model = 'MLpol', 
                     loss.type = "square", loss.gradient = T)
summary(agg.online)
plot(agg.online, pause=F)


load("ajout-guillaume1/datapropre")






