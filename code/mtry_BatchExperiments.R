# Klassifikationen
library("BatchExperiments")
setwd("/home/probst/Random_Forest/RFParset/results")
load("/home/probst/Random_Forest/RFParset/results/clas.RData")

ntree = 10000

# Example
regis = makeExperimentRegistry(id = "clas_mtry", packages="randomForest")

# Add problem
gettask = function(static, j) {
  library(OpenML)
  task = getOMLTask(task.id = static$task_id[j], verbosity=0)$input$data.set
  list(data = task$data, formula = as.formula(paste(task$target.features,"~.") ), mtry_max = static$NumberOfFeatures[j] - 1)
}

addProblem(regis, id = "taski", static = clas, dynamic = gettask, seed = 123)

forest.wrapper = function(static, dynamic, ...) {
  library(randomForest)
  err = matrix(NA, dynamic$mtry_max, 10001)
  colnames(err) = c("mtry_max", 1:10000)
  rownames(err) = 1:dynamic$mtry_max
  err[, 1] = 1:dynamic$mtry_max
  for(i in 1:dynamic$mtry_max)
    err[i, 2:10001] = randomForest(formula = dynamic$formula, data = dynamic$data, replace = TRUE, mtry = i, ...)$err.rate[,1]
  return(err)
}
addAlgorithm(regis, id = "forest", fun = forest.wrapper, overwrite = TRUE)

pars = list(j = 1:2)
task.design = makeDesign("taski", exhaustive = pars)
pars = list(ntree = ntree)
forest.design = makeDesign("forest", exhaustive = pars)

addExperiments(regis, repls = 3, prob.designs = task.design, algo.designs = list(forest.design))
summarizeExperiments(regis)
testJob(regis)
submitJobs(regis)
waitForJobs(regis)

# resi = loadResults(regis)

# Regressionen
library("BatchExperiments")
setwd("/home/probst/Random_Forest/RFParset/results")
load("/home/probst/Random_Forest/RFParset/results/reg.RData")

ntree = 10000

# Example
regis = makeExperimentRegistry(id = "reg_mtry", packages="randomForest")

# Add problem
gettask = function(static, j) {
  library(OpenML)
  task = getOMLTask(task.id = static$task_id[j], verbosity=0)$input$data.set
  list(data = task$data, formula = as.formula(paste(task$target.features,"~.") ), mtry_max = static$NumberOfFeatures[j] - 1)
}

addProblem(regis, id = "taski", static = reg, dynamic = gettask, seed = 123)

forest.wrapper = function(static, dynamic, ...) {
  library(randomForest)
  err = matrix(NA, dynamic$mtry_max, 10001)
  colnames(err) = c("mtry_max", 1:10000)
  rownames(err) = 1:dynamic$mtry_max
  err[, 1] = 1:dynamic$mtry_max
  for(i in 1:dynamic$mtry_max)
    err[i, 2:10001] = randomForest(formula = dynamic$formula, data = dynamic$data, replace = TRUE, mtry = i, ...)$mse
  return(err)
}
addAlgorithm(regis, id = "forest", fun = forest.wrapper, overwrite = TRUE)

pars = list(j = 1:2)
task.design = makeDesign("taski", exhaustive = pars)
pars = list(ntree = ntree)
forest.design = makeDesign("forest", exhaustive = pars)

addExperiments(regis, repls = 3, prob.designs = task.design, algo.designs = list(forest.design))
summarizeExperiments(regis)
testJob(regis)
submitJobs(regis)
waitForJobs(regis)

# resi = loadResults(regis)
