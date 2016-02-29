library("BatchExperiments")
setwd("/home/probst/Random_Forest/RFParset/results")
load("/home/probst/Random_Forest/RFParset/results/clas.RData")
load("/home/probst/Random_Forest/RFParset/results/reg.RData")

tasks = rbind(clas, reg)
regis = makeExperimentRegistry(id = "par_randomForest", packages=c("randomForest", "OpenML"))

# Add problem
gettask = function(static, idi) {
  task = getOMLTask(task.id = idi, verbosity=0)$input$data.set
  list(idi = idi, data = task$data, formula = as.formula(paste(task$target.features,"~.") ), 
       mtry_max = static[static$task_id == idi,]$NumberOfFeatures - 1)
}

addProblem(regis, id = "taski", static = tasks, dynamic = gettask, seed = 123)

forest.wrapper = function(static, dynamic, ...) {
  if(static[static$task_id == dynamic$idi, 2] == "Supervised Classification") {
    err = randomForest(formula = dynamic$formula, data = dynamic$data, replace = TRUE, ...)$err.rate[,1]
  } else {
    err = randomForest(formula = dynamic$formula, data = dynamic$data, replace = TRUE, ...)$mse
  }
  names(err) = 1:100
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
pars = list(ntree = 100)
forest.design.ntree = makeDesign("forest.ntree", exhaustive = pars)
pars = list(ntree = 100, nodesize = c(1:30))
forest.design.nodesize = makeDesign("forest.nodesize", exhaustive = pars)
pars = list(ntree = 100)
forest.design.mtry = makeDesign("forest.mtry", exhaustive = pars)

addExperiments(regis, repls = 100, prob.designs = task.design, algo.designs = list(forest.design.ntree))
addExperiments(regis, repls = 4, prob.designs = task.design, algo.designs = list(forest.design.nodesize))
addExperiments(regis, repls = 4, prob.designs = task.design, algo.designs = list(forest.design.mtry))

summarizeExperiments(regis)
testJob(regis)
submitJobs(regis)
waitForJobs(regis)



