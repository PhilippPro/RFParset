library("BatchExperiments")
dir = "/home/probst/Random_Forest/RFParset"
#dir = "/home/philipp/Promotion/RandomForest/RFParset/results"
setwd(paste(dir,"/results", sep = ""))
load(paste(dir,"/results/clas.RData", sep = ""))
load(paste(dir,"/results/reg.RData", sep = ""))

setConfig(conf = list(cluster.functions = makeClusterFunctionsMulticore(11)))

tasks = rbind(clas_small, reg_small)
tasks = tasks[20,]
regis = makeExperimentRegistry(id = "par_randomForest_ntree_grid", packages=c("OpenML", "mlr", "randomForest", "ranger"), 
                               work.dir = paste(dir,"/results", sep = ""), src.dirs = paste(dir,"/functions", sep = ""), seed = 1)

# Add problem
gettask = function(static, idi, rel.mtry = NULL, rel.nodesize = 0.0000001, sample.fraction = ifelse(replace, 1, 0.632), 
                   replace = TRUE, respect.unordered.factors = FALSE) {
  task = getOMLTask(task.id = idi, verbosity=0)$input$data.set
  mtry = ifelse(rel.mtry == -1, floor(sqrt(ncol(task$data))), ceiling(as.numeric(as.character(rel.mtry)) * (ncol(task$data)-1)))
  min.node.size = ifelse(rel.nodesize == -1, 1, ifelse(rel.nodesize == -5, 5, ceiling(as.numeric(as.character(rel.nodesize))*nrow(task$data))))
  
  list(idi = idi, data = task$data, formula = as.formula(paste(task$target.features,"~.") ), 
       target = task$target.features,
       mtry = mtry, 
       rel.mtry = as.numeric(as.character(rel.mtry)),
       min.node.size = min.node.size,
       rel.nodesize = as.numeric(as.character(rel.nodesize)),
       sample.fraction = sample.fraction,
       replace = replace, 
       respect.unordered.factors = respect.unordered.factors)
}
addProblem(regis, id = "taski", static = tasks, dynamic = gettask, seed = 123, overwrite = TRUE)

# Add algorithm
forest.wrapper.ntree = function(static, dynamic, size = 0, ...) {
  if(static[static$task_id == dynamic$idi, 2] == "Supervised Classification") {
    dynamic$data[,dynamic$target] = droplevels(as.factor(dynamic$data[,dynamic$target]))
    err = randomForest(formula = dynamic$formula, data = dynamic$data, replace = TRUE, ...)$err.rate[,1]
  } else {
    err = randomForest(formula = dynamic$formula, data = dynamic$data, replace = TRUE, ...)$mse
  }
  list(err = err, datainfo = c(static[static$task_id == dynamic$idi, c(1,2, 15, 13, 14, 18,19)]), 
       nodesize = dynamic$min.node.size, rel.nodesize = dynamic$rel.nodesize, mtry = dynamic$mtry)
}
addAlgorithm(regis, id = "forest.ntree", fun = forest.wrapper.ntree, overwrite = TRUE)

forest.wrapper.parset = function(static, dynamic, ...) {
  if(static[static$task_id == dynamic$idi, 2] == "Supervised Classification") {
    dynamic$data[,dynamic$target] = droplevels(as.factor(dynamic$data[,dynamic$target]))
    time = system.time(pred <- ranger(formula = dynamic$formula, data = dynamic$data, 
                                      mtry = dynamic$mtry, sample.fraction = dynamic$sample.fraction, 
                                      min.node.size = dynamic$min.node.size,
                                      replace = dynamic$replace, probability = TRUE, 
                                      respect.unordered.factors = dynamic$respect.unordered.factors, ... )$predictions)
    pred2 = factor(colnames(pred)[max.col(pred)], levels = colnames(pred))
    conf.matrix = getConfMatrix2(dynamic, pred2, relative = TRUE)
    k = nrow(conf.matrix)
    AUC = -1
    AUCtry = try(multiclass.auc2(pred, dynamic$data[,dynamic$target]))
    if(is.numeric(AUCtry))
      AUC = AUCtry
    measures = c(measureACC(dynamic$data[,dynamic$target], pred2), mean(conf.matrix[-k, k]), 
                 measureMMCE(dynamic$data[,dynamic$target], pred2), AUC)
    names(measures) = c("ACC", "BER", "MMCE", "multi.AUC")
  } else {
    time = system.time(pred <- ranger(formula = dynamic$formula, data = dynamic$data, 
                                      mtry = dynamic$mtry, sample.fraction = dynamic$sample.fraction, 
                                      min.node.size = dynamic$min.node.size, 
                                      replace = dynamic$replace, probability = TRUE, 
                                      respect.unordered.factors = dynamic$respect.unordered.factors, ... )$predictions)
    measures = c(measureMAE(dynamic$data[,dynamic$target] , pred),  measureMEDAE(dynamic$data[,dynamic$target], pred), 
                 measureMEDSE(dynamic$data[,dynamic$target], pred), measureMSE(dynamic$data[,dynamic$target], pred))
    names(measures) = c("MAE", "MEDAE", "MEDSE", "MSE")
  }
  list(measures = measures, time = time, datainfo = c(static[static$task_id == dynamic$idi, c(1, 2, 15, 13, 14, 18, 19)]), 
       algoinfo = list(nodesize = dynamic$min.node.size, rel.nodesize = dynamic$rel.nodesize, mtry = dynamic$mtry, 
                       rel.mtry = dynamic$rel.mtry, sample.fraction = dynamic$sample.fraction, replace = dynamic$replace, 
                       respect.unordered.factors = dynamic$respect.unordered.factors))
}
addAlgorithm(regis, id = "forest.parset", fun = forest.wrapper.parset, overwrite = TRUE)


# ntree
pars = list(ntree = 10000)
forest.design.ntree = makeDesign("forest.ntree", exhaustive = pars)
pars = list(idi = tasks$task_id)
task.design = makeDesign("taski", exhaustive = pars)
addExperiments(regis, repls = 30, prob.designs = task.design, algo.designs = list(forest.design.ntree)) # 1: ca. 5 Minuten

# Dataframe with gridded parameter settings, that should be tested
ps = makeParamSet(
  makeDiscreteParam("rel.nodesize", values = c(-1, -5, 0.0000001, seq(1/40, 1/4, length.out = 10))), # -1, -5 values for default, 0.0000001 for 1
  makeDiscreteParam("rel.mtry", values = c(-1, 0.0000001, seq(1/10, 1, length.out = 10))) # -1 for \sqrt(p)
)

ps = makeParamSet(
  makeNumericParam("rel.nodesize", values = c(-5, -1, 0.0000001, seq(1/40, 1/4, length.out = 10))), # -1, -5 values for default, 0.0000001 for 1
  makeDiscreteParam("rel.mtry", values = c(-1, 0.0000001, seq(1/10, 1, length.out = 10))) # -1 for \sqrt(p)
)


n = 10
grid.design = generateGridDesign(ps)
grid.design = data.frame(idi = rep(tasks$task_id, each = nrow(grid.design)), grid.design[rep(1:nrow(grid.design), nrow(tasks)) ,])

grid.design = makeDesign("taski", design = grid.design)

pars = list(num.trees = 10000)
forest.design.parset = makeDesign("forest.parset", exhaustive = pars)

# Send experiments
addExperiments(regis, repls = 1, prob.designs = grid.design, algo.designs = list(forest.design.parset)) # 1 replication enough, as rf quite stabilized at 10000 trees (see quantiles for verification)
summarizeExperiments(regis)
id = findExperiments(regis, algo.pattern = "forest.parset")
testJob(regis, id[1000])

# Chunk jobs
chunk1 = list()
for(i in 1:30)
  chunk1[[i]] = c(findExperiments(regis, algo.pattern = "forest.ntree", repls=i))
chunk2 = chunk(findExperiments(regis, algo.pattern = "forest.parset"), chunk.size = nrow(tasks))

chunks = c(chunk1, chunk2)

submitJobs(regis, ids = chunk2)

#waitForJobs(regis)

#regis = loadRegistry("/home/probst/Random_Forest/RFParset/results/par_randomForest-files")
#showStatus(regis)
rest = chunk(findNotDone(regis), chunk.size = nrow(tasks))

submitJobs(regis, ids = rest)

#res = loadResults(regis)








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


