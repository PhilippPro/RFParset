library(mlrMBO)

# Define the regression task
data(airquality)
# delete NA values
airquality = airquality[!apply(is.na(airquality),1, function(x) any(x) ),]

# Define our target function
# Measures: MSE
performan = function(x)  {
  pred <- ranger(formula = Ozone ~ . , data = airquality, mtry = x$mtry, min.node.size = x$min.node.size, 
                 sample.fraction = x$sample.fraction, replace = x$replace, num.trees = 3000)$predictions
    measureMSE(airquality$Ozone, pred)
}

# Its ParamSet
ps = makeParamSet(
  makeLogicalParam("replace"),
  makeIntegerParam("min.node.size", lower = 1, upper = 30),
  makeNumericParam("sample.fraction", lower = 0.2, upper = 1),
  makeIntegerParam("mtry", lower = 1, upper = 5)
)

# Budget
f.evals = 40
mbo.init.design.size = 30

# Focus search
infill.opt = "focussearch"
mbo.focussearch.points = 100
mbo.focussearch.maxit = 3
mbo.focussearch.restarts = 3

library(mlrMBO)
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

control = makeMBOControl(n.objectives = 1L, propose.points = mbo.prop.points, impute.y.fun = function(x, y, opt.path) 1, 
                         save.on.disk.at = 1:40, save.file.path = "/home/probst/Random_Forest/RFParset/results/optpath.RData")
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
