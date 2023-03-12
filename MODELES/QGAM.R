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

data_train<-d_ent_ouvre_24
data_test<-d_test_ouvre_24
qu_target <- seq (0.05,0.95,0.05)

equation <- Load ~ s(Load.48)+s(Temp_s95)+s(Temp_s99)+s(Temp)+s(toy)+Lundi+Mardi+Mercredi+Jeudi +s(Temp_s95_min)+s(Temp_s95_max)+s(Temp_s99_min)+s(Temp_s99_max)#+Vendredi #Je crois que si je mets tous les indicatrices de jours ca bug donc on va laisser comme ca (j'avais eu le même pb avant mais jsp comment je l'avais résolu)
qgam_l<- mqgam(form = list(equation,~ s(Temp)), data = data_train, qu = qu_target)

Viz <- getViz(qgam_l)
save(Viz,qu_target,equation,file="MODELES/qgam_midi")

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