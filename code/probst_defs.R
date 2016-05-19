load(paste0(dir,"/results/clas.RData"))
load(paste0(dir,"/results/reg.RData"))
tasks = rbind(clas_small, reg_small)

OMLDATASETS = tasks$did[1:3]

MEASURES = list(mmce, logloss, multiclass.au1p, timetrain)

LEARNERIDS = c("classif.randomForest", "classif.ranger", "classif.randomForestSRC")

DESSIZE = function(ps) {
  sum(getParamLengths(ps))
}

makeMyParamSet = function(lrn.id, task = NULL) {
  switch(lrn.id, 
         classif.ranger = makeParamSet(
           makeIntegerParam("num.trees", lower = 1, upper = 100), 
           makeNumericParam("sample.fraction", lower = 0, upper = 1),
           makeNumericParam("mtry", lower = 0, upper = 1),
           makeNumericParam("min.node.size", lower = 0, upper = 0.25)
         ),
         classif.randomForest = makeParamSet(
           makeIntegerParam("ntree", lower = 1, upper = 100), 
           makeNumericParam("replace", lower = 0, upper = 1),
           makeNumericParam("sampsize", lower = 0, upper = 1),
           makeNumericParam("mtry", lower = 0, upper = 1),
           makeNumericParam("nodesize", lower = 0, upper = 0.25),
           makeNumericParam("maxnodes", lower = 0, upper = 1)
         ),
         classif.randomForestSRC = makeParamSet(
           makeIntegerParam("ntree", lower = 1, upper = 100), 
           makeNumericParam("mtry", lower = 0, upper = 1),
           makeDiscreteParam("splitrule", values = c("gini", "gini.unwt", "gini.hvwt", "random")),
           #makeDiscreteParam("bootstrap", values = c("by.root", "by.node", "none")), 
           # not possible, as oob are not available; resample alternatively
           makeNumericParam("nodesize", lower = 0, upper = 0.25),
           makeNumericParam("nodedepth", lower = 0, upper = 1)
         )
  )
}

# FIXME: this is maybe not so good?
# mabe we would like to do this instead:
# makeNumericParam("mtry", lower = 0, upper = 1, trafo = function(mtry) mtry * p) 
# ???
CONVERTPARVAL = function(par.vals, task) {
  typ = getTaskType(task)
  n = getTaskSize(task)
  p = getTaskNFeats(task)
  if (!is.null(par.vals$replace)) par.vals$replace = as.logical(round(par.vals$replace))
  if (!is.null(par.vals$sampsize)) par.vals$sampsize = ceiling(par.vals$sampsize * n)
  if (!is.null(par.vals$mtry)) par.vals$mtry = ceiling(par.vals$mtry * p)
  if (!is.null(par.vals$nodesize)) par.vals$nodesize = min(ceiling(par.vals$nodesize * n), par.vals$sampsize) # nodesize darf nicht größer sein als sampsize!
  if (!is.null(par.vals$maxnodes)) par.vals$maxnodes = max(2, floor(par.vals$maxnodes * (par.vals$sampsize/par.vals$nodesize)))
  if (!is.null(par.vals$min.node.size)) par.vals$min.node.size =  min(ceiling(par.vals$nodesize * n), ceiling(par.vals$sample.fraction * n)) # nodesize darf nicht größer sein als sampsize!
  if (!is.null(par.vals$splitrule)) par.vals$splitrule = as.character(par.vals$splitrule)
  if (!is.null(par.vals$bootstrap)) par.vals$bootstrap = as.character(par.vals$bootstrap)
  if (!is.null(par.vals$nodedepth)) par.vals$nodedepth = ceiling(par.vals$nodedepth * floor(log(n, 2))) # nodedepth kann zwischen 1 und floor(log(n, 2)) liegen
  return(par.vals)
}


