library(BatchExperiments)
dir = "/home/probst/Random_Forest/RFParset"
#dir = "/home/philipp/Promotion/RandomForest/RFParset"
setwd(paste0(dir,"/results"))
load(paste0(dir,"/results/clas.RData"))
load(paste0(dir,"/results/reg.RData"))

setConfig(conf = list(cluster.functions = makeClusterFunctionsMulticore(10)))

tasks = rbind(clas_small, reg_small)
regis = makeExperimentRegistry(id = "par_randomForest_ntree_grid", packages=c("OpenML", "mlr", "randomForest", "ranger", "randomForestSRC", "caTools"), 
                               work.dir = paste(dir,"/results", sep = ""), src.dirs = paste(dir,"/functions", sep = ""), seed = 1)

source(paste0(dir, "/functions/gettask.R"))
addProblem(regis, id = "taski", static = tasks, dynamic = gettask, seed = 123, overwrite = TRUE)

# Add algorithms

# ntree
forest.wrapper.ntree = function(static, dynamic, size = 0, ...) {
  if(static[static$task_id == dynamic$idi, 2] == "Supervised Classification") {
    dynamic$data[,dynamic$target] = droplevels(as.factor(dynamic$data[,dynamic$target]))
    err = randomForest(formula = dynamic$formula, data = dynamic$data, replace = TRUE, ...)$err.rate[,1]
  } else {
    err = randomForest(formula = dynamic$formula, data = dynamic$data, replace = TRUE, ...)$mse
  }
  list(err = err, datainfo = c(static[static$task_id == dynamic$idi, c(1,2, 15, 13, 14, 18,19)]), 
       nodesize = dynamic$min.node.size, mtry = dynamic$mtry)
}
addAlgorithm(regis, id = "forest.ntree", fun = forest.wrapper.ntree, overwrite = TRUE)

# ntree, rfsrc
forest.wrapper.ntree.rfsrc = function(static, dynamic, size = 0, ...) {
  if(static[static$task_id == dynamic$idi, 2] == "Supervised Classification") {
    dynamic$data[,dynamic$target] = droplevels(as.factor(dynamic$data[,dynamic$target]))
    err = rfsrc(formula = dynamic$formula, data = dynamic$data, ...)$err.rate[,1]
  } else {
    err = rfsrc(formula = dynamic$formula, data = dynamic$data, ...)$err.rate
  }
  list(err = err, datainfo = c(static[static$task_id == dynamic$idi, c(1,2, 15, 13, 14, 18,19)]), 
       nodesize = dynamic$min.node.size, mtry = dynamic$mtry)
}
addAlgorithm(regis, id = "forest.ntree.rfsrc", fun = forest.wrapper.ntree.rfsrc, overwrite = TRUE)


# randomForest
forest.wrapper.randomForest1 = function(static, dynamic, ...) {
  if(static[static$task_id == dynamic$idi, 2] == "Supervised Classification") {
    dynamic$data[,dynamic$target] = droplevels(as.factor(dynamic$data[,dynamic$target]))
    time = system.time(pred <- randomForest(formula = dynamic$formula, data = dynamic$data, 
                                      mtry = dynamic$mtry, 
                                      nodesize = dynamic$min.node.size,
                                      sampsize = dynamic$sampsize,
                                      maxnodes = dynamic$maxnodes,
                                      replace = dynamic$replace,  
                                      ... ))
    pred = predict(pred, type = "prob")
    pred2 = factor(colnames(pred)[max.col(pred)], levels = colnames(pred))
    conf.matrix = getConfMatrix2(dynamic, pred2, relative = TRUE)
    k = nrow(conf.matrix)
    AUC = -1
    AUCtry = try(multiclass.au1u(pred, dynamic$data[,dynamic$target]))
    if(is.numeric(AUCtry))
      AUC = AUCtry
    measures = c(measureACC(dynamic$data[,dynamic$target], pred2), mean(conf.matrix[-k, k]), 
                 measureMMCE(dynamic$data[,dynamic$target], pred2), AUC, 
                 measureMulticlassBrier(pred, dynamic$data[,dynamic$target]), measureLogloss(pred, dynamic$data[,dynamic$target]))
    names(measures) = c("ACC", "BER", "MMCE", "multi.AUC", "multi.BRIER", "LOGLOSS")
    } else {
    time = system.time(pred <- randomForest(formula = dynamic$formula, data = dynamic$data, 
                                            mtry = dynamic$mtry, 
                                            nodesize = dynamic$min.node.size,
                                            sampsize = dynamic$sampsize,
                                            maxnodes = dynamic$maxnodes,
                                            replace = dynamic$replace, 
                                            ... )$predicted)
    measures = c(measureMAE(dynamic$data[,dynamic$target] , pred),  measureMEDAE(dynamic$data[,dynamic$target], pred), 
                 measureMEDSE(dynamic$data[,dynamic$target], pred), measureMSE(dynamic$data[,dynamic$target], pred))
    names(measures) = c("MAE", "MEDAE", "MEDSE", "MSE")
  }
  list(measures = measures, time = time, datainfo = c(static[static$task_id == dynamic$idi, c(1, 2, 15, 13, 14, 18, 19)]), 
       algoinfo = list(mtry = dynamic$mtry, rel.mtry = dynamic$rel.mtry, nodesize = dynamic$min.node.size, 
                       rel.nodesize = dynamic$rel.nodesize,  sampsize = dynamic$sampsize, 
                       sample.fraction = dynamic$sample.fraction, replace = dynamic$replace, 
                       maxnodes = dynamic$maxnodes, rel.maxnodes = dynamic$rel.maxnodes))
}
addAlgorithm(regis, id = "forest.randomForest1", fun = forest.wrapper.randomForest1, overwrite = TRUE)

# ranger
forest.wrapper.ranger = function(static, dynamic, ...) {
  if(static[static$task_id == dynamic$idi, 2] == "Supervised Classification") {
    dynamic$data[,dynamic$target] = droplevels(as.factor(dynamic$data[,dynamic$target]))
    time = system.time(pred <- ranger(formula = dynamic$formula, data = dynamic$data, 
                                      mtry = dynamic$mtry, sample.fraction = dynamic$sample.fraction, 
                                      min.node.size = dynamic$min.node.size,
                                      replace = dynamic$replace, probability = TRUE, 
                                      respect.unordered.factors = dynamic$respect.unordered.factors, 
                                      num.threads = 1, ... )$predictions)
    pred2 = factor(colnames(pred)[max.col(pred)], levels = colnames(pred))
    conf.matrix = getConfMatrix2(dynamic, pred2, relative = TRUE)
    k = nrow(conf.matrix)
    AUC = -1
    AUCtry = try(multiclass.au1u(pred, dynamic$data[,dynamic$target]))
    if(is.numeric(AUCtry))
      AUC = AUCtry
    measures = c(measureACC(dynamic$data[,dynamic$target], pred2), mean(conf.matrix[-k, k]), 
                 measureMMCE(dynamic$data[,dynamic$target], pred2), AUC, 
                 measureMulticlassBrier(pred, dynamic$data[,dynamic$target]), measureLogloss(pred, dynamic$data[,dynamic$target]))
    names(measures) = c("ACC", "BER", "MMCE", "multi.AUC", "multi.BRIER", "LOGLOSS")
  } else {
    time = system.time(pred <- ranger(formula = dynamic$formula, data = dynamic$data, 
                                      mtry = dynamic$mtry, sample.fraction = dynamic$sample.fraction, 
                                      min.node.size = dynamic$min.node.size, 
                                      replace = dynamic$replace, 
                                      respect.unordered.factors = dynamic$respect.unordered.factors, 
                                      num.threads = 1, ... )$predictions)
    measures = c(measureMAE(dynamic$data[,dynamic$target] , pred),  measureMEDAE(dynamic$data[,dynamic$target], pred), 
                 measureMEDSE(dynamic$data[,dynamic$target], pred), measureMSE(dynamic$data[,dynamic$target], pred))
    names(measures) = c("MAE", "MEDAE", "MEDSE", "MSE")
  }
  list(measures = measures, time = time, datainfo = c(static[static$task_id == dynamic$idi, c(1, 2, 15, 13, 14, 18, 19)]), 
       algoinfo = list(mtry = dynamic$mtry, rel.mtry = dynamic$rel.mtry, nodesize = dynamic$min.node.size, 
                       rel.nodesize = dynamic$rel.nodesize,  sampsize = dynamic$sampsize, sample.fraction = dynamic$sample.fraction, replace = dynamic$replace, 
                       respect.unordered.factors = dynamic$respect.unordered.factors))
}
addAlgorithm(regis, id = "forest.ranger", fun = forest.wrapper.ranger, overwrite = TRUE)

# randomForestSRC
forest.wrapper.randomForestSRC = function(static, dynamic, ...) {
  if(static[static$task_id == dynamic$idi, 2] == "Supervised Classification") {
    dynamic$data[,dynamic$target] = droplevels(as.factor(dynamic$data[,dynamic$target]))
    time = system.time(pred <- rfsrc(formula = dynamic$formula, data = dynamic$data, 
                                            bootstrap = dynamic$bootstrap,
                                            mtry = dynamic$mtry, 
                                            nodesize = dynamic$min.node.size,
                                            nodedepth = dynamic$nodedepth,
                                            splitrule = dynamic$splitrule,
                                            importance = "none",... )$predicted.oob)
    pred2 = factor(colnames(pred)[max.col(pred)], levels = colnames(pred))
    conf.matrix = getConfMatrix2(dynamic, pred2, relative = TRUE)
    k = nrow(conf.matrix)
    AUC = -1
    AUCtry = try(multiclass.au1u(pred, dynamic$data[,dynamic$target]))
    if(is.numeric(AUCtry))
      AUC = AUCtry
    measures = c(measureACC(dynamic$data[,dynamic$target], pred2), mean(conf.matrix[-k, k]), 
                 measureMMCE(dynamic$data[,dynamic$target], pred2), AUC, 
                 measureMulticlassBrier(pred, dynamic$data[,dynamic$target]), measureLogloss(pred, dynamic$data[,dynamic$target]))
    names(measures) = c("ACC", "BER", "MMCE", "multi.AUC", , "multi.BRIER", "LOGLOSS")
  } else {
    time = system.time(pred <- rfsrc(formula = dynamic$formula, data = dynamic$data, 
                                            bootstrap = dynamic$bootstrap,
                                            mtry = dynamic$mtry, 
                                            nodesize = dynamic$min.node.size,
                                            nodedepth = dynamic$nodedepth,
                                            splitrule = dynamic$splitrule,
                                            importance = "none",... )$predicted.oob)
    measures = c(measureMAE(dynamic$data[,dynamic$target] , pred),  measureMEDAE(dynamic$data[,dynamic$target], pred), 
                 measureMEDSE(dynamic$data[,dynamic$target], pred), measureMSE(dynamic$data[,dynamic$target], pred))
    names(measures) = c("MAE", "MEDAE", "MEDSE", "MSE")
  }
  list(measures = measures, time = time, datainfo = c(static[static$task_id == dynamic$idi, c(1, 2, 15, 13, 14, 18, 19)]), 
       algoinfo = list(mtry = dynamic$mtry, rel.mtry = dynamic$rel.mtry, nodesize = dynamic$min.node.size, 
                       rel.nodesize = dynamic$rel.nodesize,  sampsize = dynamic$sampsize, 
                       sample.fraction = dynamic$sample.fraction, replace = dynamic$replace, 
                       bootstrap = dynamic$bootstrap, nodedepth = dynamic$nodedepth, 
                       rel.nodedepth = dynamic$rel.nodedepth, splitrule = dynamic$splitrule))
}
addAlgorithm(regis, id = "forest.randomForestSRC", fun = forest.wrapper.randomForestSRC, overwrite = TRUE)

# ntree
pars = list(ntree = 10000)
forest.design.ntree = makeDesign("forest.ntree", exhaustive = pars)
pars = list(idi = tasks$task_id)
task.design = makeDesign("taski", exhaustive = pars)
addExperiments(regis, repls = 100, prob.designs = task.design, algo.designs = list(forest.design.ntree)) # 1: ca. 5 Minuten

# ntree with rfsrc
pars = list(ntree = 10000)
forest.design.ntree.rfsrc = makeDesign("forest.ntree.rfsrc", exhaustive = pars)
addExperiments(regis, repls = 100, prob.designs = task.design, algo.designs = list(forest.design.ntree.rfsrc)) #

ps = list()
# Dataframe with gridded parameter settings, that should be tested
ps[[1]] = makeParamSet(
  makeDiscreteParam("rel.mtry", values = c("log(p)", "sqrt(p)", "one", seq(1/10, 1, length.out = 10))),
  makeDiscreteParam("rel.nodesize", values = c("log(n)", "sqrt(n)", "one", "five", seq(1/40, 1/4, length.out = 10)))
)

ps[[2]] = makeParamSet(
  makeDiscreteParam("rel.mtry", values = c("log(p)", "sqrt(p)", "one", seq(1/10, 1, length.out = 10))), 
  makeDiscreteParam("sample.fraction", values = c(seq(1/10, 1, length.out = 10)))
)

ps[[3]] = makeParamSet(
  makeDiscreteParam("rel.nodesize", values = c("log(n)", "sqrt(n)", "one", "five", seq(1/40, 1/4, length.out = 10))), 
  makeDiscreteParam("sample.fraction", values = c(seq(1/10, 1, length.out = 10)))
)
ps[[4]] = makeParamSet(
  makeLogicalParam("replace"),
  makeDiscreteParam("sample.fraction", values = c(seq(1/40, 1, length.out = 40)))
)
ps[[5]] = makeParamSet(
  makeDiscreteParam("rel.mtry", values = c("log(p)", "sqrt(p)", "one", seq(1/10, 1, length.out = 10))),
  makeDiscreteParam("rel.maxnodes", values = c(seq(1/10, 1, length.out = 10)))
)
ps[[6]] = makeParamSet(
  makeDiscreteParam("rel.nodesize", values = c("log(n)", "sqrt(n)", "one", "five", seq(1/40, 1/4, length.out = 10))), 
  makeDiscreteParam("rel.maxnodes", values = c(seq(1/10, 1, length.out = 10)))
)
ps[[7]] = makeParamSet(
  makeDiscreteParam("sample.fraction", values = c(seq(1/40, 1, length.out = 40))),
  makeDiscreteParam("rel.maxnodes", values = c(seq(1/10, 1, length.out = 10)))
)

# rfsrc parameter erstmal univariat betrachten? (oder mit mtry/nodesize kombinieren)
ps[[8]] = makeParamSet(
  makeDiscreteParam("rel.nodesize", values = c("log(n)", "sqrt(n)", "one", "five", seq(1/40, 1/4, length.out = 10))), 
  makeDiscreteParam("bootstrap", values = c("by.root", "by.node", "none"))
)

ps[[9]] = makeParamSet(
  makeDiscreteParam("rel.mtry", values = c("log(p)", "sqrt(p)", "one", seq(1/10, 1, length.out = 10))),
  makeDiscreteParam("bootstrap", values = c("by.root", "by.node", "none"))
)

ps[[10]] = makeParamSet(
  makeDiscreteParam("rel.nodesize", values = c("log(n)", "sqrt(n)", "one", "five", seq(1/40, 1/4, length.out = 10))),
  makeDiscreteParam("splitrule", values = c("normal", "unwt", "hvwt")) # see http://www.ccs.miami.edu/~hishwaran/papers/I.ML.2015.pdf for more infos
)

ps[[11]] = makeParamSet(
  makeDiscreteParam("rel.mtry", values = c("log(p)", "sqrt(p)", "one", seq(1/10, 1, length.out = 10))),
  makeDiscreteParam("splitrule", values = c("normal", "unwt", "hvwt")) # see http://www.ccs.miami.edu/~hishwaran/papers/I.ML.2015.pdf for more infos
)

ps[[12]] = makeParamSet(
  makeDiscreteParam("splitrule", values = c("normal", "unwt", "hvwt")), # see http://www.ccs.miami.edu/~hishwaran/papers/I.ML.2015.pdf for more infos
  makeDiscreteParam("bootstrap", values = c("by.root", "by.node", "none"))
)

ps[[13]] = makeParamSet(
  makeDiscreteParam("rel.nodesize", values = c("log(n)", "sqrt(n)", "one", "five", seq(1/40, 1/4, length.out = 10))),
  makeDiscreteParam("nodedepth", values = seq(1/40, 1/4, length.out = 10))
)

ps[[14]] = makeParamSet(
  makeDiscreteParam("rel.mtry", values = c("log(p)", "sqrt(p)", "one", seq(1/10, 1, length.out = 10))),
  makeDiscreteParam("nodedepth", values = seq(1/40, 1/4, length.out = 10))
)

ps[[15]] = makeParamSet(
  makeDiscreteParam("bootstrap", values = c("by.root", "by.node", "none")),
  makeDiscreteParam("nodedepth", values = seq(1/40, 1/4, length.out = 10))
)

ps[[16]] = makeParamSet(
  makeDiscreteParam("splitrule", values = c("normal", "unwt", "hvwt")), # see http://www.ccs.miami.edu/~hishwaran/papers/I.ML.2015.pdf for more infos
  makeDiscreteParam("nodedepth", values = seq(1/40, 1/4, length.out = 10))
)

# Maximin Design

#ps_maximin = list()

#ps_maximin[[1]] = makeParamSet(
#  makeNumericParam("rel.mtry", lower = 0, upper = 1),
#  makeNumericParam("rel.nodesize", lower = 0, upper = 1/4),
#  makeNumericParam("sample.fraction", lower = 0, upper = 1/4),
#  makeNumericParam("replace", lower = 0, upper = 1)
#)

#maximin.design = maximinLHS(4 ,100,1)
#maximin.design[2:3, ] = maximin.design[2:3, ] * 1/4
#apply(maximin.design, 1, range)
#  plot(maximin.design[1,], maximin.design[2,])

#ps[[8]] = makeParamSet( 
#makeLogicalParam("respect.unordered.factors")
#)
# -> Runtime can get infeasible large, due to the missing ordering of the values!



grid.design = list()
for (i in 1:length(ps)) {
  grid.design[[i]] = generateGridDesign(ps[[i]])
  namen = colnames(grid.design[[i]])
  n_exp = nrow(grid.design[[i]])
  grid.design[[i]] = as.data.frame(grid.design[[i]][rep(1:nrow(grid.design[[i]]), nrow(tasks)) ,])
  colnames(grid.design[[i]]) = namen
  grid.design[[i]] = data.frame(idi = rep(tasks$task_id, each = n_exp), grid.design[[i]])
  grid.design[[i]] = makeDesign("taski", design = grid.design[[i]])
}

pars = list(num.trees = 10000)
forest.design.ranger = makeDesign("forest.ranger", exhaustive = pars)
pars = list(ntree = 10000)
forest.design.randomForest1 = makeDesign("forest.randomForest1", exhaustive = pars)
pars = list(ntree = 10000)
forest.design.randomForestSRC = makeDesign("forest.randomForestSRC", exhaustive = pars)

# Send experiments
for(i in 1:4)
  addExperiments(regis, repls = 1, prob.designs = grid.design[[i]], algo.designs = list(forest.design.ranger)) # 1 replication enough, as rf quite stabilized at 10000 trees (see quantiles for verification)

for(i in 1:6)
  addExperiments(regis, repls = 1, prob.designs = grid.design[[i]], algo.designs = list(forest.design.randomForest1)) # 1 replication enough, as rf quite stabilized at 10000 trees (see quantiles for verification)

for(i in c(1, 8:16))
  addExperiments(regis, repls = 1, prob.designs = grid.design[[i]], algo.designs = list(forest.design.randomForestSRC)) # 1 replication enough, as rf quite stabilized at 10000 trees (see quantiles for verification)

summarizeExperiments(regis)
id = findExperiments(regis, algo.pattern = "forest.ranger")
testJob(regis, id[55000])

# Chunk jobs
#chunk1 = list()
#for(i in 1:100)
#  chunk1[[i]] = c(findExperiments(regis, algo.pattern = "forest.ntree", repls=i))
#chunk2 = chunk(findExperiments(regis, algo.pattern = "forest.parset"), chunk.size = nrow(tasks))

chunks = chunk(findExperiments(regis), chunk.size = nrow(tasks))

submitJobs(regis, chunks)

#waitForJobs(regis)

a = chunk(findNotDone(regis), chunk.size = nrow(tasks))
submitJobs(regis, a)
#regis = loadRegistry("/home/probst/Random_Forest/RFParset/results/par_randomForest_ntree_grid-files")
#showStatus(regis)




# Anhang
# ONEDIMENSIONAL
# # nodesize
# ps = makeParamSet(
#   makeDiscreteParam("rel.nodesize", values = c(-5, -1, 0.0000001, seq(1/40, 1/4, length.out = 10))), # -1, -5 values for default, 0.0000001 for 1
#   makeDiscreteParam("rel.mtry", values = c(-1)) # -1 for \sqrt(p)
# )
# grid.design = generateGridDesign(ps)

# mtry
# ps = makeParamSet(
#   makeDiscreteParam("rel.nodesize", values = c(-1)), # -1, -5 values for default, 0.0000001 for 1
#   makeDiscreteParam("rel.mtry", values = c(-1, 0.0000001, seq(1/10, 1, length.out = 10))) # -1 for \sqrt(p)
# )
# grid.design = generateGridDesign(ps)

# # Dataframe with random parameter settings
# # restricted
# p = 10
# ps = makeParamSet(
#   makeNumericParam("rel.nodesize", lower = 0.0000001 / (10*4), upper = 1/4),
#   makeNumericParam("rel.mtry", lower = 0.0000001 , upper = 1)
# )
# 
# n = 10
# restr.design = generateRandomDesign(n = n, ps)
# restr.design = data.frame(idi = rep(tasks$task_id, each = n), restr.design[rep(1:n, nrow(tasks)) ,])
# 
# # exhaustive
# p = 10
# ps = makeParamSet(
#   makeNumericParam("rel.nodesize", lower = 0.0000001 / (10*4), upper = 1/4),
#   makeNumericParam("rel.mtry", lower = 0.00000001, upper = 1),
#   makeNumericParam("sample.fraction", lower = 0.000001, upper = 1),
#   makeLogicalParam("replace"),
#   makeLogicalParam("respect.unordered.factors")
# )
# n = 10
# exhau.design = generateRandomDesign(n = n, ps)
# exhau.design = data.frame(idi = rep(tasks$task_id, each = n), exhau.design[rep(1:n, nrow(tasks)) ,])
# 
# task.design1 = makeDesign("taski", design = restr.design)
# task.design2 = makeDesign("taski", design = exhau.design)
# pars = list(num.trees = 10000)
# forest.design.parset = makeDesign("forest.parset", exhaustive = pars)


