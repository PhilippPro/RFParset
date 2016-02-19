# Klassifikationen
library(randomForest)
library(randomForestSRC)
options(java.parameters = "- Xmx1024m") # Should avoid java gc overhead
library(OpenML)
load("/home/probst/Random_Forest/RFParset/results/clas.RData")

time1 = time2 = vector(mode = "list", length = nrow(clas))
runs1 = runs2 = vector(mode = "list", length = nrow(clas))

set.seed(1989)
for(j in 1:nrow(clas)){
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
  }
  gc()
  save(time1, time2, runs1, runs2, file = "/home/probst/Random_Forest/RFParset/results/clas_time.RData")
}

# Regressionen
load("/home/probst/Random_Forest/RFParset/results/reg.RData")

time1 = time2 = vector(mode = "list", length = nrow(clas))
runs1 = runs2 = vector(mode = "list", length = nrow(clas))

set.seed(1989)
for(j in 1:nrow(reg)){
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