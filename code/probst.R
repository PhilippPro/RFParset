library(mlr)
library(batchtools)
library(plyr)

dir = "/home/probst/Random_Forest/RFParset"
setwd(paste0(dir,"/results"))
source(paste0(dir,"/code/probst_defs.R"))

unlink("probs-muell", recursive = TRUE)
reg = makeExperimentRegistry("probs-muell",
  packages = c("mlr", "OpenML")
)

# add our selected OML dsets as problems
for (did in OMLDATASETS) {
  data = list(did = did)
  addProblem(name = as.character(did), data = data)
}

# add one generic 'algo' that evals the RF in hyperpar space
addAlgorithm("eval", fun = function(job, data, instance, lrn.id, ...) {
  par.vals = list(...)             
  par.vals = par.vals[!is.na(par.vals)]
  oml.dset = getOMLDataSet(data$did)             
  task = convertOMLDataSetToMlr(oml.dset)
  lrn = makeLearner(lrn.id, predict.type = "prob")
  par.vals = CONVERTPARVAL(par.vals, task)
  lrn = setHyperPars(lrn, par.vals = par.vals)
  mod = train(lrn, task)
  oob = getOutOfBag(mod, task)
  performance(oob, measures = MEASURES, model = mod)
})


# FIXME: we need to add the defaults of each learner and defaults that we could invent.
set.seed(124)
ades = data.frame()
for (lid in LEARNERIDS) {
  ps = makeMyParamSet(lid, task = NULL)
  des.size = DESSIZE(ps)
  d = generateDesign(des.size, ps)
  d = cbind(lrn.id = lid, d, stringsAsFactors = FALSE)
  ades = rbind.fill(ades, d)
}

addExperiments(algo.designs = list(eval = ades))

for(i in 1:48) {
  print(i)
  submitJobs(i)
}
submitJobs()
getStatus()
getErrorMessages()
res = reduceResultsDataTable(fun = function(r) as.data.frame(as.list(r)), reg = reg)
res

# zu Debugzwecken
#lrn.id = "classif.randomForest"
#par.vals = as.list(ades[1,-1])
#data$did = 457
