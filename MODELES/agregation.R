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
load("MODELES/bagging_gam.rda") # gam_bagg_i -> pred_gam_baggi
load("MODELES/boosting_arbres.rda") # xgb_i -> pred_xgb_i et pred_gbm_i
load("MODELES/stacking_forets.rda") # stack_ouvre_i -> pred_stack_ouvre_i
load("MODELES/stacking_boosting.rda") # gbm_stack_i -> pred_gbm_stack_i et pred_xgb_stack
load("MODELES/qgam.rda") # Viz_i -> 


rmse <- function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)), digits=0))
}

mape <- function(y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)), digits=2))
}


#### sur d_ent_ouvre_0

pred_qgam_0 <- sapply(Viz_0, predict, newdata = d_test_ouvre_0)

experts <- cbind(pred_gam_ouvre_0, pred_foret_ouvre_0$predictions, pred_gbm_0,
                 pred_xgb_0, pred_stack_ouvre_0$predictions, pred_gbm_stack_0,
                 pred_xgb_stack_0, pred_qgam_0[, "0.45"], pred_qgam_0[, "0.5"], pred_qgam_0[, "0.55"], pred_gam_bagg0)

colnames(experts) <- c("gam", "foret", "gbm", "xgb", "stack_foret", "stack_gbm", "stack_xgb", "qgam0.45", "qgam0.5", "qgam0.55", "gam_bagg0_1", "gam_bagg0_2", "gam_bagg0_3", "gam_bagg0_4", "gam_bagg0_5", "gam_bagg0_6", "gam_bagg0_7", "gam_bagg0_8", "gam_bagg0_9", "gam_bagg0_10")


oracle(Y=d_test_ouvre_0$Load, experts, model = "convex", loss.type = "square")

# algorithme EWA
agg.online <- mixture(Y = d_test_ouvre_0$Load , experts = experts, model = 'EWA', 
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


# faire du bagging de qgam 0.45 ?
# comprendre pourquoi les résidus dévient aussi violemment vers le bas






