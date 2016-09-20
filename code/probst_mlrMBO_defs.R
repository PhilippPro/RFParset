load("/home/probst/Random_Forest/RFParset/results/clas.RData")
load("/home/probst/Random_Forest/RFParset/results/reg.RData")
tasks = rbind(clas_small, reg_small)

OMLDATASETS = tasks$did[!(tasks$did %in% c(1054, 1071, 1065))] # Cannot guess task.type from data! for these 3

MEASURES = function(x) switch(x, "classif" = list(acc, ber, mmce, multiclass.au1u, multiclass.brier, logloss, timetrain), "regr" = list(mse, mae, medae, medse, timetrain))

LEARNERIDS = c("randomForest") # , "ranger", "randomForestSRC")

makeMyParamSet = function(lrn.id, task = NULL) {
  switch(lrn.id,
         randomForest = makeParamSet(
           makeIntegerParam("ntree", lower = 5000, upper = 5000),
           makeLogicalParam("replace"),
           makeNumericParam("sampsize", lower = 0, upper = 1),
           makeNumericParam("mtry", lower = 0, upper = 1),
           makeNumericParam("nodesize", lower = 0, upper = 0.5),
           makeNumericParam("maxnodes", lower = 0, upper = 1)
         ),
         ranger = makeParamSet(
           makeIntegerParam("num.trees", lower = 50, upper = 10000),
           makeLogicalParam("replace"),
           makeNumericParam("sample.fraction", lower = 0, upper = 1),
           makeNumericParam("mtry", lower = 0, upper = 1),
           makeNumericParam("min.node.size", lower = 0, upper = 0.5)
         ),
         randomForestSRC = makeParamSet(
           makeIntegerParam("ntree", lower = 50, upper = 10000),
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

CONVERTPARVAL = function(par.vals, task, lrn.id) {
  typ = getTaskType(task)
  n = getTaskSize(task)
  p = getTaskNFeats(task)
  if (lrn.id == "ranger") {
    par.vals$sample.fraction = max(par.vals$sample.fraction, 1/n) # sollte nicht kleiner als "1" Beobachtung sein
    par.vals$mtry = ceiling(par.vals$mtry * p)
    par.vals$min.node.size =  ceiling(par.vals$min.node.size * ceiling(par.vals$sample.fraction * n)) # nodesize darf nicht größer sein als sampsize!
  }
  if (lrn.id == "randomForest") {
    par.vals$sampsize = max(ceiling(par.vals$sampsize * n), 1)
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
  }
  return(par.vals)
}

makeRLearner.classif.forestMBO = function() {
  makeRLearnerClassif(
    cl = "classif.forestMBO",
    package = "MASS",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "f.evals", lower = 1, upper = Inf, default = 3),
      makeIntegerLearnerParam(id = "mbo.init.design", lower = 1, upper = Inf, default = 20)
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob"),
    name = "Random Forest MBO-tuned",
    short.name = "rfMBO"
  )
}

trainLearner.classif.forestMBO = function(.learner, .task, .subset, .weights = NULL, ...) {
  forestMBO(task = .task, ...)
}

predictLearner.classif.forestMBO = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newdata = .newdata, ...)
  if (.learner$predict.type == "response") 
    return(getPredictionResponse(p)) else return(getPredictionProbabilities(p))
}

forestMBO = function(task, lrn.id = "randomForest", f.evals = 100, mbo.init.design.size = 20) { # this is overfitting!
  print(f.evals)
  type = getTaskType(task)
  measures = MEASURES(type)
  ps = makeMyParamSet(lrn.id)
  
  performan = function(x) {
    par.vals = x[!(is.na(x))]
    par.vals = CONVERTPARVAL(par.vals, task, lrn.id)
    lrn.id = paste0(type, ".", lrn.id)
    lrn = switch(type, "classif" = makeLearner(lrn.id, predict.type = "prob"), "regr" = makeLearner(lrn.id))
    lrn = setHyperPars(lrn, par.vals = par.vals)
    mod = train(lrn, task)
    oob = getOOBPreds(mod, task)
    performance(oob, measures = measures, model = mod)
  }
  
  # Budget
  f.evals = f.evals #100
  mbo.init.design.size = mbo.init.design.size
  
  # Focus search
  infill.opt = "focussearch"
  mbo.focussearch.points = 2000
  mbo.focussearch.maxit = 3
  mbo.focussearch.restarts = 3
  
  # The final SMOOF objective function
  objFun = makeMultiObjectiveFunction(
    name = "reg",
    fn = performan,
    par.set = ps,
    has.simple.signature = FALSE,
    noisy = TRUE,
    n.objectives = 1,
    minimize = c(TRUE)
  )
  
  # Build the control object
  mbo.prop.points = 1
  mbo.crit = "cb"
  crit.cb.pi = 0.5
  
  control = makeMBOControl(n.objectives = 1L, propose.points = mbo.prop.points, impute.y.fun = function(x, y, opt.path) median(data.frame(opt.path)$y))
  control = setMBOControlTermination(control, max.evals = f.evals, iters = 100)
  control = setMBOControlInfill(control, crit = mbo.crit, opt = infill.opt,
                                opt.focussearch.maxit = mbo.focussearch.maxit,
                                opt.focussearch.points = mbo.focussearch.points,
                                opt.restarts = mbo.focussearch.restarts,
                                crit.cb.pi = crit.cb.pi, crit.cb.lambda = NULL)
  
  #if (dynamic$learner == "randomForest") {
  mbo.learner = makeLearner("regr.randomForest", predict.type = "se")
  #} else {
  #  mbo.learner = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")
  #}
  
  design = generateDesign(mbo.init.design.size, smoof::getParamSet(objFun), fun = lhs::maximinLHS)
  
  result = mbo(fun = objFun, design = design, learner = mbo.learner, control = control)
  res = data.frame(result$opt.path)
  best = res[res$y == min(res$y),]
  best = best[nrow(best),] # take the last, if several ones are equally best
  
  best = best[colnames(best) %in% names(ps$pars)]
  best = as.list(best)
  par.vals = CONVERTPARVAL(best, task, lrn.id)
  lrn.id = paste0(type, ".", lrn.id)
  lrn = switch(type, "classif" = makeLearner(lrn.id, predict.type = "prob"), "regr" = makeLearner(lrn.id))
  lrn = setHyperPars(lrn, par.vals = par.vals)
  train(lrn, task)
}


# train.set = seq(1, 150, 2)
# test.set = seq(2, 150, 2)
# model = train("classif.forestMBO", iris.task, subset = train.set)
# p = predict(model, newdata = iris, subset = test.set)



# forest.mbo = function(static, dynamic, ...) {
#   # Define our target function
#   # Measures: MSE
#   
#   dynamic$data[,dynamic$target] = droplevels(as.factor(dynamic$data[,dynamic$target]))
#   n = nrow(dynamic$data)
#   p = ncol(dynamic$data) - 1
#   
#   # jeweils für alle 4 Maße definieren bzw. für Regression und Klassifikation
#   performan = function(x)  {
#     replace = as.logical(round(x$replace))
#     if(static[static$task_id == dynamic$idi, 2] == "Supervised Classification"){
#       pred <- ranger(formula = dynamic$formula, data = dynamic$data,
#                      mtry = x$mtry, sample.fraction = x$sample.fraction,
#                      min.node.size = x$min.node.size,
#                      replace = replace,
#                      num.threads = 1, num.trees = 5000)$predictions
#       measureMMCE(dynamic$data[,dynamic$target], pred)
#     } else {
#       pred <- ranger(formula = dynamic$formula, data = dynamic$data,
#                      mtry = x$mtry, sample.fraction = x$sample.fraction,
#                      min.node.size = x$min.node.size,
#                      replace = replace,
#                      num.threads = 1, num.trees = 5000)$predictions
#       measureMSE(dynamic$data[,dynamic$target], pred)
#     }
#   }
#   # Its ParamSet
#   up.node.size = as.numeric(ceiling(n / 4))
#   ps = makeParamSet(
#     makeNumericParam("replace", lower = 0, upper = 1),
#     makeIntegerParam("min.node.size", lower = 1, upper = up.node.size),
#     makeNumericParam("sample.fraction", lower = 0.2, upper = 1),
#     makeIntegerParam("mtry", lower = 1, upper = p)
#   )
#   
#   # Budget
#   f.evals = 100
#   mbo.init.design.size = 20
#   
#   # Focus search
#   infill.opt = "focussearch"
#   mbo.focussearch.points = 2000
#   mbo.focussearch.maxit = 3
#   mbo.focussearch.restarts = 3
#   
#   # The final SMOOF objective function
#   objFun = makeMultiObjectiveFunction(
#     name = "reg",
#     fn = performan,
#     par.set = ps,
#     has.simple.signature = FALSE,
#     noisy = TRUE,
#     n.objectives = 1,
#     minimize = c(TRUE)
#   )
#   
#   # Build the control object
#   mbo.prop.points = 1
#   mbo.crit = "cb"
#   crit.cb.pi = 0.5
#   
#   control = makeMBOControl(n.objectives = 1L, propose.points = mbo.prop.points, impute.y.fun = function(x, y, opt.path) median(data.frame(opt.path)$y))
#   control = setMBOControlTermination(control, max.evals = f.evals, iters = 100)
#   control = setMBOControlInfill(control, crit = mbo.crit, opt = infill.opt,
#                                 opt.focussearch.maxit = mbo.focussearch.maxit,
#                                 opt.focussearch.points = mbo.focussearch.points,
#                                 opt.restarts = mbo.focussearch.restarts,
#                                 crit.cb.pi = crit.cb.pi, crit.cb.lambda = NULL)
#   
#   if (dynamic$learner == "randomForest") {
#     mbo.learner = makeLearner("regr.randomForest", predict.type = "se")
#   } else {
#     mbo.learner = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")
#   }
#   
#   design = generateDesign(mbo.init.design.size, smoof::getParamSet(objFun), fun = lhs::maximinLHS)
#   
#   result = mbo(fun = objFun, design = design, learner = mbo.learner, control = control)
#   res = data.frame(result$opt.path)
#   best = res[res$y == min(res$y),]
#   best[nrow(best),]
# }