
forest.wrapper.mbo = function(static, dynamic, ...) {
# Define our target function
# Measures: MSE
  
  x = list(mtry = 1, sample.fraction=1, min.node.size = 1, replace = FALSE)
  dynamic$data[,dynamic$target] = droplevels(as.factor(dynamic$data[,dynamic$target]))
  
  
  # jeweils für alle 4 Maße definieren bzw. für Regression und Klassifikation
performan = function(x)  {
  if(static[static$task_id == dynamic$idi, 2] == "Supervised Classification"){
pred <- ranger(formula = dynamic$formula, data = dynamic$data,
                                    mtry = x$mtry, sample.fraction = x$sample.fraction, 
                                    min.node.size = x$min.node.size,
                                    replace = x$replace, 
                                    num.threads = 1, num.trees =5000)$predictions
  measureMMCE(dynamic$data[,dynamic$target], pred)
  }
} else {
  pred <- ranger(formula = dynamic$formula, data = dynamic$data,
                 mtry = x$mtry, sample.fraction = x$sample.fraction, 
                 min.node.size = x$min.node.size,
                 replace = x$replace, 
                 num.threads = 1, num.trees =5000)$predictions
  measureMSE(dynamic$data[,dynamic$target], pred)
}

# Its ParamSet
ps = makeParamSet(
  makeLogicalParam("replace"),
  makeIntegerParam("min.node.size", lower = 1, upper = ceiling(c(dynamic$n) / 4)),
  makeNumericParam("sample.fraction", lower = 0.2, upper = 1),
  makeIntegerParam("mtry", lower = 1, upper = dynamic$p)
)

# Budget
f.evals = 40
mbo.init.design.size = 30

# Focus search
infill.opt = "focussearch"
mbo.focussearch.points = 100
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
method = "parego"
if (method == "parego") {
  mbo.prop.points = 1
  mbo.crit = "cb"
  parego.crit.cb.pi = 0.5
}

control = makeMBOControl(n.objectives = 1L, propose.points = mbo.prop.points, impute.y.fun = function(x, y, opt.path) median(data.frame(opt.path)$y))
control = setMBOControlTermination(control, max.evals =  f.evals, iters = 300)
control = setMBOControlInfill(control, crit = mbo.crit, opt = infill.opt,
                              opt.focussearch.maxit = mbo.focussearch.maxit,
                              opt.focussearch.points = mbo.focussearch.points,
                              opt.restarts = mbo.focussearch.restarts,
                              crit.cb.pi = parego.crit.cb.pi, crit.cb.lambda = NULL)

mbo.learner = makeLearner("regr.randomForest", predict.type = "se")

design = generateDesign(mbo.init.design.size, smoof::getParamSet(objFun), fun = lhs::maximinLHS)

set.seed(123)
result = mbo(fun = objFun, design = design, learner = mbo.learner, control = control)
res = data.frame(result$opt.path)
best = res[res$y == min(res$y),]
best[nrow(best),]
}
