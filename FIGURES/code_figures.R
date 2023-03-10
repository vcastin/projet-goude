rm(list=objects())

load("DONNEES/data.rda")

############# affichage conso avril 2012
n <- 48*(59+32)
m <- 48*(59+32+30)
d_plot <- d[n:m,]

plot(d_plot$Date, d_plot$Load, xlab = "Jour (avril 2012)", ylab = "Consommation (MWh)", type="l")

############## affichage conso mardi à 12h

d_plot <- filter(d, Year >= 2013 & Year <= 2016 & WeekDays == "Tuesday" & tod == 24)
plot(d_plot$Date, d_plot$Load, xlab = "Mardi à 12h", ylab = "Consommation (MWh)", type="l")

############## corrélation entre les variables

d_plot <- select(d, Load, Load.48, Temp, Temp_s95, tod, toy, Year)
d_plot <- filter(d_plot, Year <= 2019)
d_plot <- select(d_plot, Load, Load.48, Temp, Temp_s95, tod, toy)
colnames(d_plot) <- c("Load", "Load.48", "Temp", "Temp_s95", "tod", "toy")
corrplot(cor(d_plot), 
         type = "upper",
         method = "circle",
         order = "hclust",
         hclust.method = "ward.D",
         title = "Corrélation des variables (période 2012 - 2019)",
         mar = c(2, 1, 3, 1))

############## choix de mtry pour les forêts

grid_mtry <-  expand.grid(mtry = seq(1,10,by=1), 
                          min.node.size = 5,
                          splitrule = "variance")

fitControl <- trainControl(method="cv", number=16,
                           verboseIter = TRUE)

fit = caret::train(
  x = d_ent_ouvre_1,  # est-ce qu'il ne faut pas enlever Load ?
  y = d_ent_ouvre_1$Load,
  method = 'ranger',
  num.trees = 100,
  tuneGrid = grid_mtry,
  trControl = fitControl)

#print(fit)

p <- plot(fit, main = "Choix du paramètre mtry par validation croisée \n Jeu de données : d_ent_ouvre_1", xlab = "mtry", ylab="RMSE")
print(p)

############## choix de min.node.size pour les forets

grid_minnode <-  expand.grid(mtry = 8, 
                             min.node.size = seq(1, 10, by = 1),
                             splitrule = "variance")

fitControl <- trainControl(method="cv", number=16,
                           verboseIter = TRUE)

fit = caret::train(
  x = d_ent_ouvre_1,
  y = d_ent_ouvre_1$Load,
  method = 'ranger',
  num.trees = 100,
  tuneGrid = grid_minnode,
  trControl = fitControl)

#print(fit)

p <- plot(fit, main = "Choix du paramètre min.node.size par validation croisée \n Jeu de données : d_ent_ouvre_1", xlab = "min.node.size", ylab="RMSE")
print(p)

############### choix ntree pour les forets

error_list <- c()
ntree_list <- seq(25,400,25)

# For each number of trees...
for(n in ntree_list){
  #print(n) # to check the loop
  
  #ptm <- proc.time() # evaluate execution time
  
  # Grow a forest with n trees
  res <- ranger(formule_1, 
                data=d_ent_ouvre_1, 
                num.trees = n,
                mtry=8,
                importance='permutation')
  
  #temp <- proc.time() - ptm
  
  #time_list <- c(time_list, temp[3])
  
  error_list <- c(error_list, res$prediction.error)
}

# plotting results


p <- plot(ntree_list, error_list, 
          main = "RMSE en fonction du nombre d'arbres \n Jeu de données : d_ent_ouvre_1", 
          xlab = "ntree", 
          ylab="RMSE",
          type = "o",
          pch=18,
          col="blue")


print(p)
