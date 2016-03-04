# Klassifikationen
library(randomForest)
library(randomForestSRC)
library(ranger)
options(java.parameters = "- Xmx1024m") # Should avoid java gc overhead
library(OpenML)
load("/home/probst/Random_Forest/RFParset/results/clas.RData")

time1 = time2 = time3 = vector(mode = "list", length = nrow(clas))
runs1 = runs2 = runs3 = vector(mode = "list", length = nrow(clas))

set.seed(1989) # 260 dauert sehr lange!, 265 und 266 auch!
for(j in 1:250){
  print(j)
  task = try(getOMLTask(task.id = clas$task_id[j], verbosity=0))
  if(substr(task[1],1,5) != "Error"){
  # zu Factor umwandeln, damit rfsrc damit umgehen kann
  if(any(sapply(task$input$data.set$data, class) == "character")){
    for(k in which(sapply(task$input$data.set$data, class) == "character"))
      task$input$data.set$data[,k] = as.factor(task$input$data.set$data[,k])
  }
  # randomForest kann mit zu vielen Kategorien nicht umgehen!; randomForestSRC schon.
  time1[[j]] = system.time(runs1[[j]] <- try(randomForest(as.formula(paste(task$input$data.set$target.features,"~.") ), data = task$input$data.set$data, replace = TRUE, ntree = 10000)$err.rate[, 1]))
  time2[[j]] = system.time(runs2[[j]] <- try(rfsrc(as.formula(paste(task$input$data.set$target.features,"~.") ), data = task$input$data.set$data, replace = TRUE, ntree = 10000, importance = "none")$err.rate[,1]))
  time3[[j]] = system.time(runs3[[j]] <- try(ranger(as.formula(paste(task$input$data.set$target.features,"~.") ), data = task$input$data.set$data, replace = TRUE, num.trees = 10000)$prediction.error))
  }
  gc()
  # save(time1, time2, time3, runs1, runs2, runs3, file = "/home/probst/Random_Forest/RFParset/results/clas_time2.RData")
   save(time1, time2, runs1, runs2, file = "/home/probst/Random_Forest/RFParset/results/clas_time.RData")
}
load("/home/probst/Random_Forest/RFParset/results/clas_time.RData")

# Probleme wenn Number of Instances zu groß ist, Algorithmus braucht zu lange, bekommt nicht genug Speicher.

Reduce(sum, lapply(time1,"[", 1)) # User
Reduce(sum, lapply(time1,"[", 2)) # System
Reduce(sum, lapply(time1,"[", 3)) # verstrichen
# insgesamt 2/8 h (rfsrc dauert länger)

hist(unlist(lapply(time1,"[", 3)))
max(unlist(lapply(time1,"[", 3))) # 7894 Sek. (bestimmte Ausreißer könnte man rauslassen) 
plot(unlist(lapply(time1,"[", 3)))

plot(clas$NumberOfInstances[1:250], unlist(lapply(time1[1:250],"[", 3)))
plot(clas$NumberOfFeatures[1:250], unlist(lapply(time1[1:250],"[", 3)))
plot(clas$NumberOfFeatures[1:250] * clas$NumberOfInstances[1:250], unlist(lapply(time1[1:250],"[", 3)))

cor(clas$NumberOfInstances[1:250], unlist(lapply(time1[1:250],"[", 3)))
cor(clas$NumberOfFeatures[1:250], unlist(lapply(time1[1:250],"[", 3)))
# It only depends on NumberOfInstances

library(rgl)
plot3d(clas$NumberOfInstances[1:250], clas$NumberOfFeatures[1:250], 
       unlist(lapply(time1[1:250],"[", 3)), col="red", size=3,
       xlab = "n", ylab = "p", zlab = "time") 

Reduce(sum, lapply(time2,"[", 1)) # User
Reduce(sum, lapply(time2,"[", 2)) # System
Reduce(sum, lapply(time2,"[", 3)) # verstrichen
# insgesamt 2/8 h (rfsrc dauert länger)

hist(unlist(lapply(time2,"[", 3)))
max(unlist(lapply(time2,"[", 3))) # 7894 Sek. (bestimmte Ausreißer könnte man rauslassen) 
plot(unlist(lapply(time2,"[", 3)))

plot(clas$NumberOfInstances[1:250], unlist(lapply(time2[1:250],"[", 3)))
plot(clas$NumberOfFeatures[1:250], unlist(lapply(time2[1:250],"[", 3)))
plot(clas$NumberOfFeatures[1:250] * clas$NumberOfInstances[1:250], unlist(lapply(time2[1:250],"[", 3)))

cor(clas$NumberOfInstances[1:250], unlist(lapply(time2[1:250],"[", 3)))
cor(clas$NumberOfFeatures[1:250], unlist(lapply(time2[1:250],"[", 3)))
# It only depends on NumberOfInstances

library(rgl)
plot3d(clas$NumberOfInstances[1:250], clas$NumberOfFeatures[1:250], 
       unlist(lapply(time2[1:250],"[", 3)), col="red", size=3,
       xlab = "n", ylab = "p", zlab = "time") 

small_time1 = time1[which(clas$NumberOfInstances < 1000 & clas$NumberOfFeatures < 1000 & clas$NumberOfClasses < 30)]
Reduce(sum, lapply(small_time1,"[", 3)) # 1000 Sekunden verstrichen (ca. 16 Minuten)
plot.ecdf(unlist(lapply(small_time1,"[", 3)))

# Regressionen
load("/home/probst/Random_Forest/RFParset/results/reg.RData")

time1 = time2 = vector(mode = "list", length = nrow(reg))
runs1 = runs2 = vector(mode = "list", length = nrow(reg))

set.seed(1989)
for(j in 1:135){
  print(j)
  task = try(getOMLTask(task.id = reg$task_id[j], verbosity=0))
  # randomForest kann mit zu vielen Kategorien nicht umgehen!; randomForestSRC schon.
  if(substr(task[1],1,5) != "Error"){
  time1[[j]] = system.time(runs1[[j]] <- try(randomForest(as.formula(paste(task$input$data.set$target.features,"~.") ), data = task$input$data.set$data, replace = TRUE, ntree = 10000)$mse))
  time2[[j]] = system.time(runs2[[j]] <- try(rfsrc(as.formula(paste(task$input$data.set$target.features,"~.") ), data = task$input$data.set$data, replace = TRUE, ntree = 10000, importance = "none")$err.rate))
  }
  gc()
  save(time1, time2, runs1, runs2, file = "/home/probst/Random_Forest/RFParset/results/reg_time.RData")
}
load("/home/probst/Random_Forest/RFParset/results/reg_time.RData")

Reduce(sum, lapply(time1,"[", 1)) # User
Reduce(sum, lapply(time1,"[", 2)) # System
Reduce(sum, lapply(time1,"[", 3)) # verstrichen
# insgesamt 2/8 h (rfsrc dauert länger)

hist(unlist(lapply(time1,"[", 3)))
max(unlist(lapply(time1,"[", 3))) # 7894 Sek. (bestimmte Ausreißer könnte man rauslassen) 
plot(unlist(lapply(time1,"[", 3)))

plot(reg$NumberOfInstances[1:135], unlist(lapply(time1[1:135],"[", 3)))
plot(reg$NumberOfFeatures[1:135], unlist(lapply(time1[1:135],"[", 3)))
plot(reg$NumberOfFeatures[1:135] * reg$NumberOfInstances[1:135], unlist(lapply(time1[1:135],"[", 3)))

cor(reg$NumberOfInstances[1:135], unlist(lapply(time1[1:135],"[", 3)))
cor(reg$NumberOfFeatures[1:135], unlist(lapply(time1[1:135],"[", 3)))
# It only depends on NumberOfInstances

library(rgl)
plot3d(reg$NumberOfInstances[1:135], reg$NumberOfFeatures[1:135], 
       unlist(lapply(time1[1:135],"[", 3)), col="red", size=3,
       xlab = "n", ylab = "p", zlab = "time") 

Reduce(sum, lapply(time2,"[", 1)) # User
Reduce(sum, lapply(time2,"[", 2)) # System
Reduce(sum, lapply(time2,"[", 3)) # verstrichen
# insgesamt 2/8 h (rfsrc dauert länger)

hist(unlist(lapply(time2,"[", 3)))
max(unlist(lapply(time2,"[", 3))) # 7894 Sek. (bestimmte Ausreißer könnte man rauslassen) 
plot(unlist(lapply(time2,"[", 3)))

plot(reg$NumberOfInstances[1:135], unlist(lapply(time2[1:135],"[", 3)))
plot(reg$NumberOfFeatures[1:135], unlist(lapply(time2[1:135],"[", 3)))
plot(reg$NumberOfFeatures[1:135] * reg$NumberOfInstances[1:135], unlist(lapply(time2[1:135],"[", 3)))

cor(reg$NumberOfInstances[1:135], unlist(lapply(time2[1:135],"[", 3)))
cor(reg$NumberOfFeatures[1:135], unlist(lapply(time2[1:135],"[", 3)))
# It only depends on NumberOfInstances

library(rgl)
plot3d(reg$NumberOfInstances[1:135], reg$NumberOfFeatures[1:135], 
       unlist(lapply(time2[1:135],"[", 3)), col="red", size=3,
       xlab = "n", ylab = "p", zlab = "time") 

small_time1 = time1[which(reg$NumberOfInstances < 1000 & reg$NumberOfFeatures < 1000)]
Reduce(sum, lapply(small_time1,"[", 3)) # 446 Sekunden verstrichen (ca. 7,5 Minuten)
plot.ecdf(unlist(lapply(small_time1,"[", 3)))
