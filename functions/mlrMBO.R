
forest.wrapper.mbo = function(static, dynamic, ...) {
  # Define our target function
  # Measures: MSE
  
  dynamic$data[,dynamic$target] = droplevels(as.factor(dynamic$data[,dynamic$target]))
  n = nrow(dynamic$data)
  p = ncol(dynamic$data) - 1
  
  # jeweils für alle 4 Maße definieren bzw. für Regression und Klassifikation
  performan = function(x)  {
    replace = as.logical(round(x$replace))
    if(static[static$task_id == dynamic$idi, 2] == "Supervised Classification"){
      pred <- ranger(formula = dynamic$formula, data = dynamic$data,
                     mtry = x$mtry, sample.fraction = x$sample.fraction,
                     min.node.size = x$min.node.size,
                     replace = replace,
                     num.threads = 1, num.trees = 5000)$predictions
      measureMMCE(dynamic$data[,dynamic$target], pred)
  } else {
    pred <- ranger(formula = dynamic$formula, data = dynamic$data,
                   mtry = x$mtry, sample.fraction = x$sample.fraction,
                   min.node.size = x$min.node.size,
                   replace = replace,
                   num.threads = 1, num.trees = 5000)$predictions
    measureMSE(dynamic$data[,dynamic$target], pred)
  }
  }
  # Its ParamSet
  up.node.size = as.numeric(ceiling(n / 4))
  ps = makeParamSet(
    makeNumericParam("replace", lower = 0, upper = 1),
    makeIntegerParam("min.node.size", lower = 1, upper = up.node.size),
    makeNumericParam("sample.fraction", lower = 0.2, upper = 1),
    makeIntegerParam("mtry", lower = 1, upper = p)
  )
  
  # Budget
  f.evals = 100
  mbo.init.design.size = 20
  
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
  
  if (dynamic$learner == "randomForest") {
    mbo.learner = makeLearner("regr.randomForest", predict.type = "se")
  } else {
    mbo.learner = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")
  }
  
  design = generateDesign(mbo.init.design.size, smoof::getParamSet(objFun), fun = lhs::maximinLHS)
  
  result = mbo(fun = objFun, design = design, learner = mbo.learner, control = control)
  res = data.frame(result$opt.path)
  best = res[res$y == min(res$y),]
  best[nrow(best),]
}
