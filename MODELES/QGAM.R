rm(list=objects())
###############packages

library(dygraphs)
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

rmse <- function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)), digits=0))
}

mape <- function(y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)), digits=2))
}

qu_target <- seq (0.05,0.95,0.05)

data_train_0 <- d_ent_ouvre_0
data_test_0 <- d_test_ouvre_0
equation <- Load ~ s(Load.48)+s(Temp_s95)+s(Temp_s99)+s(Temp)+s(toy)+Lundi+Mardi+Mercredi+Jeudi+s(Temp_s95_min)+s(Temp_s95_max)+s(Temp_s99_min)+s(Temp_s99_max)#+Vendredi #Je crois que si je mets tous les indicatrices de jours ca bug donc on va laisser comme ca (j'avais eu le même pb avant mais jsp comment je l'avais résolu)

qgam_l_0 <- mqgam(form = list(equation,~ s(Temp)), data = data_train_0, qu = qu_target)

Viz_0 <- getViz(qgam_l_0)

data_train_12 <- d_ent_ouvre_12
data_test_12 <- d_test_ouvre_12

qgam_l_12 <- mqgam(form = list(equation,~ s(Temp)), data = data_train_12, qu = qu_target)

Viz_12 <- getViz(qgam_l_12)

save(Viz_0, Viz_12, qu_target, equation, file="MODELES/qgam.rda")

# 
# for(i in c(1:H))
# {
#   assign(paste("gam_ouvre_cr", i, sep="_"), qgam(equation, data=eval(parse(text=paste("d_ent_ouvre", i, sep="_")))))
# }
# 
# # calcul des prédictions
# for(i in c(1:H))
# {
#   assign(paste("pred_qgam_ouvre", i, sep="_"), predict(eval(parse(text=paste("qgam_ouvre_cr", i, sep="_"))), newdata = eval(parse(text=paste("d_test_ouvre", i, sep="_")))))
# }
# 
# save(list = ls(all = TRUE), file= "MODELES/GAM.rda")