# Klassifikationen
library("BatchExperiments")
setwd("/home/probst/Random_Forest/RFParset/results")
load("/home/probst/Random_Forest/RFParset/results/clas.RData")

ntree = 10000

# Example
regis = makeExperimentRegistry(id = "clas_ntree", packages="randomForest")

# Add problem
gettask = function(static, j) {
  library(OpenML)
  task = getOMLTask(task.id = static$task_id[j], verbosity=0)$input$data.set
  list(data = task$data, formula = as.formula(paste(task$target.features,"~.") ))
}

addProblem(regis, id = "taski", static = clas, dynamic = gettask, seed = 123)

forest.wrapper = function(static, dynamic, ...) {
  library(randomForest)
  err = randomForest(formula = dynamic$formula, data = dynamic$data, replace = TRUE, ...)$err.rate[,1]
  names(err) = 1:10000
  err
}
addAlgorithm(regis, id = "forest", fun = forest.wrapper, overwrite = TRUE)

pars = list(j = 1:nrow(reg))
task.design = makeDesign("taski", exhaustive = pars)
pars = list(ntree = ntree)
forest.design = makeDesign("forest", exhaustive = pars)

addExperiments(regis, repls = 100, prob.designs = task.design, algo.designs = list(forest.design))
summarizeExperiments(regis)
testJob(regis)
submitJobs(regis)
waitForJobs(regis)

# res = reduceResultsExperiments(regis)
# print(res[c(1:10),1:10 ])

# library("data.table")
# res1 = data.table(res)
# vars = names(res)[7:10006]
# res1 = res1[, lapply(.SD, mean, na.rm=TRUE), by="j", .SDcols=vars ]

# Regressionen
library("BatchExperiments")
setwd("/home/probst/Random_Forest/RFParset/results")
load("/home/probst/Random_Forest/RFParset/results/reg.RData")

ntree = 10000

# Example
regis = makeExperimentRegistry(id = "reg_ntree", packages="randomForest")

# Add problem
gettask = function(static, j) {
  library(OpenML)
  task = getOMLTask(task.id = static$task_id[j], verbosity=0)$input$data.set
  list(data = task$data, formula = as.formula(paste(task$target.features,"~.") ))
}

addProblem(regis, id = "taski", static = reg, dynamic = gettask, seed = 123)

forest.wrapper = function(static, dynamic, ...) {
  library(randomForest)
  err = randomForest(formula = dynamic$formula, data = dynamic$data, replace = TRUE, ...)$mse
  names(err) = 1:10000
  err
}
addAlgorithm(regis, id = "forest", fun = forest.wrapper, overwrite = TRUE)

pars = list(j = 1:nrow(reg))
task.design = makeDesign("taski", exhaustive = pars)
pars = list(ntree = ntree)
forest.design = makeDesign("forest", exhaustive = pars)

addExperiments(regis, repls = 100, prob.designs = task.design, algo.designs = list(forest.design))
summarizeExperiments(regis)
testJob(regis)
submitJobs(regis)
waitForJobs(regis)

reduce = function(job, res) {
  list(res[1:100])
}

# res = reduceResultsExperiments(regis)
# print(res[c(1:10),1:10 ])

# library("data.table")
# res1 = data.table(res)
# vars = names(res)[7:10006]
# res1 = res1[, lapply(.SD, mean, na.rm=TRUE), by="j", .SDcols=vars ]

