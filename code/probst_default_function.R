setwd("/nfsmb/koll/probst/Random_Forest/RFParset/results/")
setwd("/home/philipp/Promotion/RandomForest/RFParset/results")
library(data.table)

load("/home/probst/Random_Forest/RFParset/results/clas.RData")
load("/home/probst/Random_Forest/RFParset/results/reg.RData")
tasks = rbind(clas_small, reg_small)
tasks = tasks[, c("task_id", "task_type", "did", "name", "NumberOfInstances", "NumberOfFeatures", "NumberOfClasses",        
                  "MajorityClassSize", "NumberOfNumericFeatures", "NumberOfSymbolicFeatures")]
tasks$task_type = substr(tasks$task_type, 12, 1000)
library(data.table)
tasks = data.table(tasks)
tasks = tasks[!(tasks$did %in% c(1054, 1071, 1065))]
tasks$did = 1:298
tasks[, task_id := NULL]
tasks[, name := NULL]
#keys = c("task_type", "NumberOfInstances", "NumberOfFeatures")
#setkeyv(tasks, keys)


load("/nfsmb/koll/probst/Random_Forest/RFParset/results/results.RData")

hyp_par = hyp_par[1:5760, 15:27, with = F]
hyp_par$hyp_id = 1:5760

res_classif[,timetrain := NULL]
res_classif$did = ceiling(res_classif$job.id / 5760)
res_classif$hyp_id = res_classif$job.id%%5760
res_classif[hyp_id ==0]$hyp_id = 5760

res_classif$algo = NA
res_classif$algo[which(res_classif$job.id %in% c(rep(seq(1,1077120, 5760), each=1920) + rep(0:1919, 187)))] = "randomForest"
res_classif$algo[which(res_classif$job.id %in% c(rep(seq(1921,1077120, 5760), each=1920) + rep(0:1919, 187)))] = "ranger"
res_classif$algo[which(res_classif$job.id %in% c(rep(seq(3841,1077120, 5760), each=1920) + rep(0:1919, 187)))] = "randomForestSRC"

res_regr[,timetrain := NULL]
res_regr$did = ceiling(res_regr$job.id / 5760)
res_regr$hyp_id = res_regr$job.id%%5760
res_regr[hyp_id ==0]$hyp_id = 5760

save(tasks, hyp_par, res_classif, res_regr, file = "results_compressed.RData")
load("results_compressed.RData")

# random forests includen

param_randomForest = list(c("ntree", "mtry", "nodesize", "maxnodes"), c("sampsize", "replace"))
param_ranger = list(c("num.trees", "mtry", "min.node.size"), c("sample.fraction", "replace"))
param_randomForestSRC = list(c("ntree", "mtry", "nodesize", "nodedepth", "splitrule"), c("sampsize", "samptype"))

models = list()
algor = "randomForest"
k = "acc"
param = param_randomForest
library(mlr)

for(i in 1:187){
  print(i)
  data = res_classif[res_classif$did == i & res_classif$algo == algor ]
  ids = data$job.id
  data = cbind(data[, k, with = F], hyp_par[hyp_par$hyp_id %in% data$hyp_id, unlist(param), with = F])
  #data[, param[[1]][1] := as.numeric(data[[param[[1]][1]]]), with = FALSE]
  data[, param[[2]][2] := as.factor(data[[param[[2]][2]]]), with = FALSE]
  data = data[which(!is.na(data[, k, with = F]))]
  task = makeRegrTask(id = "rf", data = data, target = k)
  lrn = makeLearner(paste0("regr.", algor)) #, predict.type = "se")
  models[[i]] = train(lrn, task)
}

# optim anwenden (kann nur eindimensional optimieren)
save(models, file = "/home/probst/Random_Forest/RFParset/results/models.RData")
rm(models)

opti = function(a) {
  theta = data.frame(ntree = 5147, mtry = 0.380, nodesize = 0.005, maxnodes = 1, 
            sampsize = 0.605, replace = factor(FALSE, levels = c(TRUE, FALSE))) 
  pred = 0
  print("durchlauf")
  for(i in 1:187){
    theta[2] = (a[1] * unlist(tasks[i, 3, with =F]) + a[2] * unlist(tasks[i, 4, with =F])) / unlist(tasks[i, 4, with =F])  # a[1] * sqrt(unname(unlist(tasks[i, 4, with =F])))/ unname(unlist(tasks[i, 4, with =F]))
    # theta[3] = a[2]
    pred = pred + predict(models[[i]], newdata = theta)$data$response
  }
  pred = pred/187
  -pred
}

a = c(0.001, 0.38)

# nur n, p zur Optimierung verwenden!
res = optim(a, opti, lower = c(0.001,0.001), upper = c(1, 1))

opti(a)
opti(res$par)
# vierte Nachkommastelle um 1 verbessert

res$par


# genetic programming approach 

opti_gp = function(f) {
  theta = data.frame(ntree = 5147, mtry = 0.380, nodesize = 0.005, maxnodes = 1, 
                     sampsize = 0.605, replace = factor(FALSE, levels = c(TRUE, FALSE))) 
  pred = 0
  print("durchlauf")
  for(i in 1:187){
    theta[2] = max(min(f(unname(unlist(tasks[i, 4, with =F])))/ unname(unlist(tasks[i, 4, with =F])) , 1) , 0) #, unname(unlist(tasks[i, 3, with =F])))
    #print(f(unname(unlist(tasks[i, 4, with =F]))) / unname(unlist(tasks[i, 4, with =F])))
    #print(theta[2])
    pred = pred + predict(models[[i]], newdata = theta)$data$response
  }
  pred = pred/187
  print(f)
  print(pred)
  -pred
}

library(rgp)
functionSet1 = functionSet("+", "*", "-", "sqrt", "log", "/")
inputVariableSet1 = inputVariableSet("x")
constantFactorySet1 = constantFactorySet(function() rnorm(1))
fitnessFunction1 = function(f) opti_gp(f)
set.seed(121)
gpResult = geneticProgramming(functionSet = functionSet1,
                                          inputVariables = inputVariableSet1,
                                          constantSet = constantFactorySet1, # Was sollte man hier einstellen?
                                          fitnessFunction = fitnessFunction1,
                                          stopCondition = makeTimeStopCondition(5 * 60))


# (example of tutorial)
library(rgp)
functionSet1 = functionSet("+", "*", "-")
inputVariableSet1 = inputVariableSet("x")
constantFactorySet1 = constantFactorySet(function() rnorm(1))
interval1 = seq(from = -pi, to = pi, by = 0.1)
fitnessFunction1 = function(f) rmse(f(interval1), sin(interval1))
set.seed(1)
gpResult1 = geneticProgramming(functionSet = functionSet1,
                                    inputVariables = inputVariableSet1,
                                    constantSet = constantFactorySet1,
                                    fitnessFunction = fitnessFunction1,
                                    stopCondition = makeTimeStopCondition(5 * 60))
bestSolution1 = gpResult1$population[[which.min(gpResult1$fitnessValues)]]

plot(y = bestSolution1(interval1), x = interval1, type = "l",
       lty = 1, xlab = "x", ylab = "y")
lines(y = sin(interval1), x = interval1, lty = 2)

geneticProgramming
res_classif
