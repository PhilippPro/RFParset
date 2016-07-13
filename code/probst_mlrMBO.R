library(mlr)
library(batchtools)
library(plyr)
library(mlrMBO)

dir = "/home/probst/Random_Forest/RFParset"
setwd(paste0(dir,"/results"))
source(paste0(dir,"/code/probst_mlrMBO_defs.R"))

unlink("probst-mlrMBO", recursive = TRUE)
regis = makeExperimentRegistry("probst-mlrMBO", 
                               packages = c("mlr", "mlrMBO", "OpenML", "methods"), 
                               source = "/nfsmb/koll/probst/Random_Forest/RFParset/code/probst_mlrMBO_defs.R",
                               work.dir = "/nfsmb/koll/probst/Random_Forest/RFParset/results",
                               conf.file = "/nfsmb/koll/probst/Random_Forest/RFParset/code/.batchtools.conf.R"
)
regis$cluster.functions = makeClusterFunctionsMulticore() 

# add selected OML datasets as problems
for (did in OMLDATASETS) {
  data = list(did = did)
  addProblem(name = as.character(did), data = data)
}

# add one generic 'algo' that evals the RF in hyperpar space
addAlgorithm("eval", fun = function(job, data, instance, lrn.id, ...) {
  oml.dset = getOMLDataSet(data$did)
  task = convertOMLDataSetToMlr(oml.dset)
  type = getTaskType(task)
  ps = makeMyParamSet(lrn.id)
  lrn.id = paste0(type, ".", lrn.id)
  lrn = switch(type, "classif" = makeLearner(lrn.id, predict.type = "prob"), "regr" = makeLearner(lrn.id))
  measures = MEASURES(type)
  
  forest.mbo(task, lrn, ps, type, measures)
})


ades = data.frame()
for (lid in LEARNERIDS) {
  d = cbind(lrn.id = lid)
  ades = rbind(ades, d)
}
addExperiments(algo.designs = list(eval = ades))


summarizeExperiments()
ids = chunkIds(findNotDone(), chunk.size = 1000)

submitJobs(ids)

data$did = 457
lrn.id = "randomForest"
