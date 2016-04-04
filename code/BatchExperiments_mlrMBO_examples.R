library(mlrMBO)

learner_km = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")



# Budget
f.evals = 24#200L
mbo.init.design.size = 20L

# Focus search
infill.opt = "focussearch"
mbo.focussearch.points = 1000L
mbo.focussearch.maxit = 3L
mbo.focussearch.restarts = 3L

# smsego
sms.prop.points = 1L
dib.indicator = "sms"

# parego
parego.prop.points = 4L
parego.crit = "cb"
parego.crit.cb.pi = 0.5

# Resample Parameters (small datasets (n < 200) and "large" data sets (n > 1000!)
stratify.resampling = TRUE
nb.folds = 10L
nb.folds.reps = 10L

# MBO Learner Params
mbo.learner.id = "regr.km"
learner.params = list(covtype = "matern3_2", nugget = 1e-4)

# The MLR Learner
classif.lrn = makeLearner("classif.svm", predict.type = "response", kernel = "radial")

# Its ParamSet
svm.ps = makeParamSet(
  makeNumericParam("cost", lower = -15L, upper = 15L, trafo = function(x) 2^x),
  makeNumericParam("gamma", lower = -15L, upper = 15L, trafo = function(x) 2^x)
)

# Define the classification task
library(mlbench)
data(Ionosphere)
classif.task = makeClassifTask(id = "myIonosphere", data = Ionosphere, target = "Class")

if (nb.folds.reps == 1L) {
  rdesc = makeResampleDesc("CV", predict = "test", iters = n.folds, stratify = stratify.resampling)
} else {
  rdesc = makeResampleDesc("RepCV", predict = "test", folds = nb.folds,
                           reps = nb.folds.reps, stratify = stratify.resampling)
}
rin = makeResampleInstance(rdesc, task = classif.task)

# Define our target functio
# 2 Measures: Specificity, Sensitivity. Yeah, not really usefull ...
resampleFun = function(x)  {
  # Set the new hyperparams
  new.learner = setHyperPars(classif.lrn, par.vals = as.list(x))
  
  r = resample(new.learner, classif.task, rin, show.info = FALSE, measures = list(
    setAggregation(tnr, test.join),
    setAggregation(tpr, test.join))
  )
  r$aggr
}

library(mlrMBO)
# The final SMOOF objective function
objFun = makeMultiObjectiveFunction(
  name = "Resampler",
  fn = resampleFun,
  par.set = svm.ps,
  has.simple.signature = FALSE,
  noisy = FALSE,
  n.objectives = 2L,
  minimize = c(FALSE, FALSE)
)

method = "parego"
# Build the control object
if (method == "parego") {
  mbo.prop.points = parego.prop.points
  mbo.crit = parego.crit
}
if (method == "dib") {
  mbo.prop.points = sms.prop.points
  mbo.crit = "dib"
}

control = makeMBOControl(n.objectives = 2L, propose.points = mbo.prop.points)
control = setMBOControlTermination(control, max.evals =  f.evals, iters = 300L)
control = setMBOControlInfill(control, crit = mbo.crit, opt = infill.opt,
                              opt.focussearch.points = mbo.focussearch.points,
                              opt.focussearch.maxit = mbo.focussearch.maxit,
                              opt.restarts = mbo.focussearch.restarts,
                              crit.cb.pi = parego.crit.cb.pi, crit.cb.lambda = NULL)
control = setMBOControlMultiCrit(control, method = method, dib.indicator = dib.indicator)

mbo.learner = do.call(makeLearner, c(cl = mbo.learner.id, predict.type = "se", learner.params))

design = generateDesign(mbo.init.design.size, smoof::getParamSet(objFun), fun = lhs::maximinLHS)

result = mbo(fun = objFun, design = design, learner = mbo.learner, control = control)

return(list(
  set = result$pareto.set,
  front = result$pareto.front,
  opt.path = result$opt.path
)) 

# Du willst den RandomForest mit Hilfe von mlrMBO optimieren? Guck mal in den testfiles von mlr. Da gibt es ein Beispiel. 

context("tuneMBO")

test_that("tuneMBO", {
  skip_on_cran() # FIXME remove if mbo is on cran
  skip_if_not_installed("mlrMBO")
  attachNamespace("mlrMBO")
  # FIXME change when mlrMBO is on cran
  #requirePackagesOrSkip("!mlrMBO")
  res = makeResampleDesc("Holdout")
  ps = makeParamSet(
    makeNumericParam("cp", lower = 0, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 20)
  )
  
  n1 = 10; n2 = 2;
  mbo.ctrl = makeMBOControl(save.on.disk.at = integer(0L))
  mbo.ctrl = setMBOControlTermination(mbo.ctrl, iters = n2)
  des = generateDesign(n1, ps, fun = lhs::maximinLHS)
  ctrl = makeTuneControlMBO(learner = makeLearner("regr.lm"), mbo.control = mbo.ctrl, mbo.design = des)
  tr = tuneParams(makeLearner("classif.rpart"), multiclass.task, res, par.set = ps, control = ctrl)
  expect_equal(getOptPathLength(tr$opt.path), n1+n2)
  expect_equal(dim(as.data.frame(tr$opt.path)), c(n1 + n2, 2 + 1 + 4))
  
  ps = makeParamSet(
    makeNumericParam("sigma", lower = -10, upper = -1, trafo = function(x) 2^x)
  )
  des = generateDesign(n1, ps, fun = lhs::maximinLHS)
  ctrl = makeTuneControlMBO(learner = makeLearner("regr.lm"), mbo.control = mbo.ctrl, mbo.design = des)
  tr = tuneParams("classif.ksvm", multiclass.task, res, par.set = ps, control = ctrl)
  expect_equal(getOptPathLength(tr$opt.path), n1 + n2)
  expect_true(is.list(tr$x) && all(names(tr$x) == "sigma"))
  expect_true(tr$x$sigma > 0)
  df1 = as.data.frame(tr$opt.path)
  df2 = as.data.frame(trafoOptPath(tr$opt.path))
  # deactivate because we store trafo'ed values in opt.path
  # expect_true(all(df1$sigma < 0))
  expect_true(all(df2$sigma > 0))
  
  ps = makeParamSet(
    makeIntegerParam("ntree", lower = 10, upper = 50),
    makeNumericVectorParam("cutoff", len = 3, lower = 0.001, upper = 1, trafo = function(x) 0.9*x/sum(x))
  )
  ctrl$mbo.design = generateDesign(n1, ps, fun = lhs::maximinLHS)
  tr = tuneParams("classif.randomForest", multiclass.task, res, par.set = ps, control = ctrl)
  expect_equal(getOptPathLength(tr$opt.path), n1 + n2)
})

