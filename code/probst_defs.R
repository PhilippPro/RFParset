load(paste0(dir,"/results/clas.RData"))
load(paste0(dir,"/results/reg.RData"))
tasks = rbind(clas_small, reg_small)

OMLDATASETS = tasks$did[1:3]

MEASURES = list(mmce, timetrain)

LEARNERIDS = c("classif.randomForest")

DESSIZE = function(ps) {
  sum(getParamLengths(ps))
}

makeMyParamSet = function(lrn.id, task = NULL) {
  switch(lrn.id, 
    classif.randomForest = makeParamSet(
      makeIntegerParam("ntree", lower = 1, upper = 100), 
      makeNumericParam("mtry", lower = 0, upper = 1)
    )
  )
}

# FIXME: this is maybe not so good?
# mabe we would like to do this instead:
# makeNumericParam("mtry", lower = 0, upper = 1, trafo = function(mtry) mtry * p) 
# ???
CONVERTPARVAL = function(par.vals, task) {
  p = getTaskNFeats(task)
  par.vals$mtry = ceiling(par.vals$mtry * p)
  return(par.vals)
}


