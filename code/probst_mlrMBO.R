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
  rf = makeLearner("classif.randomForest", par.vals = list(ntree = 5000), predict.type = "prob")
  learners = list(makeLearner("classif.randomForest"), makeLearner("classif.forestMBO"))
  type = getTaskType(task)
  measures = MEASURES(type)
  rdesc = makeResampleDesc("RepCV", folds = 10, reps = 10, stratify = FALSE)
  configureMlr(on.learner.error = "warn", show.learner.output = FALSE)
  bmr = benchmark(learners, task, rdesc, measures, keep.pred = FALSE, models = FALSE, show.info = TRUE)
  bmr
})


ades = data.frame()
for (lid in LEARNERIDS) {
  d = cbind(lrn.id = lid)
  ades = rbind(ades, d)
}
addExperiments(algo.designs = list(eval = ades))

submitJobs(1)

# 5 h für einen Durchlauf bei 5(fold)*2 Evaluationen bei den 8 kleinsten Datensätzen

getStatus()
getErrorMessages()

summarizeExperiments()
ids = chunkIds(findNotDone()[1:10], chunk.size = 1)
submitJobs(ids)

min(getJobStatus()$started, na.rm = T)
max(getJobStatus()$done, na.rm = T)

res = reduceResultsList(ids = 1:10, fun = function(r) as.list(r), reg = regis)

# testing
data$did = 457
lrn.id = "randomForest"
