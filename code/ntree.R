library(randomForest)
load("/home/probst/Random_Forest/RFParset/results/clas.RData")

runs = vector(mode = "list", length = nrow(clas))
wdh = rep(10, nrow(clas)) # Zu verändern

set.seed(1989)
for(j in 1:250){ # Zu verändern
  print(j)
  task = try(getOMLTask(task.id = clas$task_id[j], verbosity=0))
  # zu Factor umwandeln, damit rfsrc damit umgehen kann
  if(any(sapply(task$input$data.set$data, class) == "character")){
    for(k in which(sapply(task$input$data.set$data, class) == "character"))
      task$input$data.set$data[,k] = as.factor(task$input$data.set$data[,k])
  }
  # randomForest kann mit zu vielen Kategorien nicht umgehen!; randomForestSRC schon.
  runs[[j]] = matrix(NA, wdh[j], 10000) 
  for(i in 1:wdh[j]){
  runs[[j]][i,] <- try(randomForest(as.formula(paste(task$input$data.set$target.features,"~.") ), data = task$input$data.set$data, replace = TRUE, ntree = 10000)$err.rate[,1])
  save(runs, file = "/home/probst/Random_Forest/RFParset/results/clas_ntree.RData")
  gc()
  }
}

# Regressionen
load("/home/probst/Random_Forest/RFParset/results/reg.RData")

runs = vector(mode="list", length=nrow(reg))
wdh = rep(10, nrow(reg)) # Zu verändern

set.seed(1989)
for(j in 1:133){ # Zu verändern
  print(j)
  task = try(getOMLTask(task.id = reg$task_id[j], verbosity=0))
  runs[[j]] = matrix(NA, wdh[j], 10000) 
  for(i in 1:wdh[j]){
    runs[[j]][i,] <- try(randomForest(as.formula(paste(task$input$data.set$target.features,"~.") ), data = task$input$data.set$data, replace = TRUE, ntree = 10000)$mse)
    save(runs, file = "/home/probst/Random_Forest/RFParset/results/reg_ntree.RData")
    gc()
}
}
