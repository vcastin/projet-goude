
rm(list=objects())

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
library(dygraphs)

set.seed(1)

d <- readRDS("Data_RTE_janv2012_oc2022.RDS")
d_plot <- select(d, Load, Load.48, Temp, Temp_s95, tod, toy, Year)
d_plot <- filter(d_plot, Year <= 2019)
d_plot <- select(d_plot, Load, Load.48, Temp, Temp_s95, tod, toy)
colnames(d_plot) <- c("Load", "Load.48", "Temp", "Temp_s95", "tod", "toy")
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


rmse <- function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)), digits=0))
}

mape <- function(y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)), digits=2))
}

Nblock<-10
borne_block<-seq(1, nrow(d_ent_ouvre), length=Nblock+1)%>%floor
block_list<-list()
l<-length(borne_block)
for(i in c(2:(l-1)))
{
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]]<-c(borne_block[l-1]:(borne_block[l]))



bs_list<-list("tp","cr","cc")
tableau<-list()
i<-1
for (bs in bs_list) {
  result_bloc<-0
  j<-1
  for (block in block_list){
    equation<-Load~s(Load.48, k=3, bs=bs)+s(Temp_s95_max, k=5, bs=bs)+s(Temp_s99_max, k=5, bs=bs)+s(Temp_s95_min, k=5, bs=bs)+s(Temp_s99_min, k=5, bs=bs)+s(Temp_s95, k=5, bs=bs)+s(Temp_s99, k=5, bs=bs)+WeekDays+s(tod, k=24, bs=)+s(toy,k=14,bs=bs)+Christmas_break+Summer_break
    
    g<-gam(equation,data=d_ent_ouvre[-block,])
    forecast<-predict(g, newdata=d_ent_ouvre[block,])
    result_bloc<-result_bloc+rmse(d_ent_ouvre[block,]$Load-forecast)
    print(bs)
  }
  tableau[[i]]<-result_bloc/10
  i<-i+1         
}
tableau<-as.data.frame(tableau)

colnames(tableau)<-bs_list
rownames(tableau)<-("RMSE")

save(tableau,file="A_AFFICHER/splines_basis")