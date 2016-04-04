library(mlrMBO)
library(mlr)

#learner_km = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

n1 = 10; n2 = 2;
mbo.ctrl = makeMBOControl(save.on.disk.at = integer(0L))
mbo.ctrl = setMBOControlTermination(mbo.ctrl, iters = n2)


ctrl = makeTuneControlMBO(learner = makeLearner("regr.lm"), mbo.control = mbo.ctrl, mbo.design = des)

ps = makeParamSet(
  makeIntegerParam("ntree", lower = 10, upper = 50),
  makeNumericVectorParam("cutoff", len = 3, lower = 0.001, upper = 1, trafo = function(x) 0.9*x/sum(x))
)
ctrl$mbo.design = generateDesign(n1, ps, fun = lhs::maximinLHS)

tr = tuneParams("classif.randomForest", multiclass.task, res, par.set = ps, control = ctrl)
