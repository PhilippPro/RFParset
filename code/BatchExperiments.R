library("BatchExperiments")
dir = "/home/probst/Random_Forest/RFParset/results"
#dir = "/home/philipp/Promotion/RandomForest/RFParset/results"
setwd(dir)
load(paste(dir,"/clas.RData", sep = ""))
load(paste(dir,"/reg.RData", sep = ""))

setConfig(conf = list(cluster.functions = makeClusterFunctionsMulticore(9)))

tasks = rbind(clas_small, reg_small)
regis = makeExperimentRegistry(id = "par_randomForest", packages=c("randomForest", "OpenML"))

# Add problem
gettask = function(static, idi) {
  task = getOMLTask(task.id = idi, verbosity=0)$input$data.set
  list(idi = idi, data = task$data, formula = as.formula(paste(task$target.features,"~.") ), 
       mtry_max = static[static$task_id == idi,]$NumberOfFeatures - 1)
}

addProblem(regis, id = "taski", static = tasks, dynamic = gettask, seed = 123)

forest.wrapper = function(static, dynamic, size = 0, ...) {
  if(static[static$task_id == dynamic$idi, 2] == "Supervised Classification") {
    err = randomForest(formula = dynamic$formula, data = dynamic$data, replace = TRUE, ...)$err.rate[,1]
  } else {
    err = randomForest(formula = dynamic$formula, data = dynamic$data, replace = TRUE, ...)$mse
  }
  names(err) = 1:10000
  list(err = err, datainfo = c(static[static$task_id == dynamic$idi, c(1,2, 15, 13, 14, 18,19)]))
}
addAlgorithm(regis, id = "forest.ntree", fun = forest.wrapper, overwrite = TRUE)
addAlgorithm(regis, id = "forest.nodesize", fun = forest.wrapper, overwrite = TRUE)

forest.wrapper.mtry = function(static, dynamic, ...) {
  library(randomForest)
  err = matrix(NA, dynamic$mtry_max, 10001)
  colnames(err) = c("mtry_max", 1:10000)
  rownames(err) = 1:dynamic$mtry_max
  err[, 1] = 1:dynamic$mtry_max
  for(i in 1:dynamic$mtry_max)
    err[i, 2:10001] = randomForest(formula = dynamic$formula, data = dynamic$data, replace = TRUE, mtry = i, ...)$mse
  return(err)
}
addAlgorithm(regis, id = "forest.mtry", fun = forest.wrapper.mtry, overwrite = TRUE)

pars = list(idi = tasks$task_id)
task.design = makeDesign("taski", exhaustive = pars)
pars = list(ntree = 10000)
forest.design.ntree = makeDesign("forest.ntree", exhaustive = pars)
pars = list(ntree = 10000, nodesize = c(1,2,3,4,5,7,10,15,20,25,30))
forest.design.nodesize = makeDesign("forest.nodesize", exhaustive = pars)
pars = list(ntree = 10000)
forest.design.mtry = makeDesign("forest.mtry", exhaustive = pars)

addExperiments(regis, repls = 30, prob.designs = task.design, algo.designs = list(forest.design.ntree)) # 12.5 h 
addExperiments(regis, repls = 4, prob.designs = task.design, algo.designs = list(forest.design.nodesize)) # 16.5 h
addExperiments(regis, repls = 4, prob.designs = task.design, algo.designs = list(forest.design.mtry)) # 33.33 h

summarizeExperiments(regis)
testJob(regis)

submitJobs(regis)
#waitForJobs(regis)

regis = loadRegistry("/home/probst/Random_Forest/RFParset/results/par_randomForest-files")
showStatus(regis)

