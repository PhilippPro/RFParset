load(paste0(dir,"/results/clas.RData"))
load(paste0(dir,"/results/reg.RData"))
tasks = rbind(clas_small, reg_small)

OMLDATASETS = tasks$did[c(1:3, 195)]

MEASURES = function(x) switch(x, "classif" = list(acc, ber, mmce, multiclass.au1u, multiclass.brier, logloss, timetrain), "regr" = list(mse, mae, medae, medse, timetrain))

LEARNERIDS = c("randomForest", "ranger", "randomForestSRC")

DESSIZE = function(ps) {
  sum(getParamLengths(ps))
}

# FIXME: Überlegen, ob Parameternamen gleicher Parameter über Learner hinweg gleich sein sollen
makeMyParamSet = function(lrn.id, task = NULL) {
  switch(lrn.id, 
         ranger = makeParamSet(
           makeIntegerParam("num.trees", lower = 1, upper = 100), 
           makeNumericParam("replace", lower = 0, upper = 1),
           makeNumericParam("sample.fraction", lower = 0, upper = 1),
           makeNumericParam("mtry", lower = 0, upper = 1),
           makeNumericParam("min.node.size", lower = 0, upper = 0.5)
         ),
         randomForest = makeParamSet(
           makeIntegerParam("ntree", lower = 1, upper = 100), 
           makeNumericParam("replace", lower = 0, upper = 1),
           makeNumericParam("sampsize", lower = 0, upper = 1),
           makeNumericParam("mtry", lower = 0, upper = 1),
           makeNumericParam("nodesize", lower = 0, upper = 0.5),
           makeNumericParam("maxnodes", lower = 0, upper = 1)
         ),
         randomForestSRC = makeParamSet(
           makeIntegerParam("ntree", lower = 1, upper = 100), 
           makeDiscreteParam("samptype", values = c("swr", "swor")), # entspricht replace
           makeNumericParam("sampsize", lower = 0, upper = 1), 
           makeNumericParam("mtry", lower = 0, upper = 1),
           makeNumericParam("nodesize", lower = 0, upper = 0.5),
           makeNumericParam("nodedepth", lower = 0, upper = 1),
           makeDiscreteParam("splitrule", values = c("normal", "unwt", "hvwt", "random"))
           #makeDiscreteParam("bootstrap", values = c("by.root", "by.node", "none")), 
           # not possible, as oob are not available; resample alternatively
         )
  )
}

# FIXME: this is maybe not so good?
# mabe we would like to do this instead:
# makeNumericParam("mtry", lower = 0, upper = 1, trafo = function(mtry) mtry * p) 
# ???
CONVERTPARVAL = function(par.vals, task, lrn.id) {
  typ = getTaskType(task)
  n = getTaskSize(task)
  p = getTaskNFeats(task)
  if (lrn.id == "ranger") {
    par.vals$sample.fraction = max(par.vals$sample.fraction, 1/n) # sollte nicht kleiner als "1" Beobachtung sein
    par.vals$replace = as.logical(round(par.vals$replace))
    par.vals$mtry = ceiling(par.vals$mtry * p)
    par.vals$min.node.size =  ceiling(par.vals$min.node.size * ceiling(par.vals$sample.fraction * n)) # nodesize darf nicht größer sein als sampsize!
  }
  if (lrn.id == "randomForest") {
    par.vals$replace = as.logical(round(par.vals$replace))
    par.vals$sampsize = ceiling(par.vals$sampsize * n)
    par.vals$mtry = ceiling(par.vals$mtry * p)
    par.vals$nodesize = ceiling(par.vals$nodesize * par.vals$sampsize) # nodesize darf nicht größer sein als sampsize!
    par.vals$maxnodes = max(2, floor(par.vals$maxnodes * (par.vals$sampsize/par.vals$nodesize)))
  }
  if (lrn.id == "randomForestSRC") {
    par.vals$sampsize = ceiling(par.vals$sampsize * n)
    par.vals$samptype = as.character(par.vals$samptype)
    par.vals$mtry = ceiling(par.vals$mtry * p)
    par.vals$nodesize = ceiling(par.vals$nodesize * par.vals$sampsize) # nodesize darf nicht größer sein als sampsize!
    par.vals$nodedepth = ceiling(par.vals$nodedepth * floor(log(n, 2))) # nodedepth kann zwischen 1 und floor(log(n, 2)) liegen
    
    # par.vals$bootstrap = as.character(par.vals$bootstrap)
    if (typ == "classif") par.vals$splitrule = switch(as.character(par.vals$splitrule), normal = "gini", unwt = "gini.unwt", hvwt = "gini.hvwt", random = "random")
    if (typ == "regr") par.vals$splitrule = switch(as.character(par.vals$splitrule), normal = "mse", unwt = "mse.unwt", hvwt = "mse.hvwt", random = "random")
     #   makeDiscreteParam("splitrule", values = c("gini", "gini.unwt", "gini.hvwt", "random"))
    #par.vals$splitrule = as.character(par.vals$splitrule)
  }
  return(par.vals)
}


