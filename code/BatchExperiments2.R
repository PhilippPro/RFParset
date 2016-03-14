library("BatchExperiments")
dir = "/home/probst/Random_Forest/RFParset"
#dir = "/home/philipp/Promotion/RandomForest/RFParset/results"
setwd(paste(dir,"/results", sep = ""))
load(paste(dir,"/results/clas.RData", sep = ""))
load(paste(dir,"/results/reg.RData", sep = ""))

setConfig(conf = list(cluster.functions = makeClusterFunctionsMulticore(10)))

tasks = rbind(clas_small, reg_small)
regis = makeExperimentRegistry(id = "par_randomForest2", packages=c("OpenML", "mlr", "randomForest", "ranger"), 
                               work.dir = paste(dir,"/results", sep = ""), src.dirs = paste(dir,"/functions", sep = ""))

# Add problem
gettask = function(static, idi, rel.nodesize) {
  task = getOMLTask(task.id = idi, verbosity=0)$input$data.set
  list(idi = idi, data = task$data, formula = as.formula(paste(task$target.features,"~.") ), 
       target = task$target.features, mtry_max = static[static$task_id == idi,]$NumberOfFeatures - 1, 
       min.node.size = ceiling(rel.nodesize*nrow(task$data)),
       rel.nodesize = rel.nodesize)
}
addProblem(regis, id = "taski", static = tasks, dynamic = gettask, seed = 123)

# Add Algorithms
forest.wrapper.ntree = function(static, dynamic, size = 0, ...) {
  dynamic$data[,dynamic$target] = droplevels(as.factor(dynamic$data[,dynamic$target]))
  if(static[static$task_id == dynamic$idi, 2] == "Supervised Classification") {
    err = randomForest(formula = dynamic$formula, data = dynamic$data, replace = TRUE, ...)$err.rate[,1]
  } else {
    err = randomForest(formula = dynamic$formula, data = dynamic$data, replace = TRUE, ...)$mse
  }
  names(err) = 1:10000
  list(err = err, datainfo = c(static[static$task_id == dynamic$idi, c(1,2, 15, 13, 14, 18,19)]))
}
addAlgorithm(regis, id = "forest.ntree", fun = forest.wrapper.ntree, overwrite = TRUE)

forest.wrapper.parset = function(static, dynamic, ...) {
  if(static[static$task_id == dynamic$idi, 2] == "Supervised Classification") {
    dynamic$data[,dynamic$target] = droplevels(as.factor(dynamic$data[,dynamic$target]))
    pred = ranger(formula = dynamic$formula, data = dynamic$data, replace = TRUE, probability = TRUE, 
                  min.node.size = dynamic$min.node.size, ... )$predictions
    pred2 = factor(colnames(pred)[max.col(pred)], levels = colnames(pred))
    conf.matrix = getConfMatrix2(dynamic, pred2, relative = TRUE)
    k = nrow(conf.matrix)
    AUC = -1
    AUCtry = try(multiclass.auc2(pred, pred2))
    if(is.numeric(AUCtry))
      AUC = AUCtry
    measures = c(measureACC(dynamic$data[,dynamic$target], pred2), mean(conf.matrix[-k, k]), 
                 measureMMCE(dynamic$data[,dynamic$target], pred2), AUC)
    names(measures) = c("ACC", "BER", "MMCE", "multi.AUC")
  } else {
    pred = ranger(formula = dynamic$formula, data = dynamic$data, replace = TRUE, min.node.size = dynamic$min.node.size, ...)$predictions
    measures = c(measureMAE(dynamic$data[,dynamic$target] , pred),  measureMEDAE(dynamic$data[,dynamic$target], pred), 
                 measureMEDSE(dynamic$data[,dynamic$target], pred), measureMSE(dynamic$data[,dynamic$target], pred))
    names(measures) = c("MAE", "MEDAE", "MEDSE", "MSE")
  }
  list(measures = measures, datainfo = c(static[static$task_id == dynamic$idi, c(1, 2, 15, 13, 14, 18, 19)]), 
       nodesize = dynamic$min.node.size, rel.nodesize = dynamic$rel.nodesize)
}
addAlgorithm(regis, id = "forest.parset", fun = forest.wrapper.parset, overwrite = TRUE)

# Für ntree
pars = list(ntree = 10000)
forest.design.ntree = makeDesign("forest.ntree", exhaustive = pars)

# Mache ein Dataframe mit allen Parametereinstellungen, die ausprobiert werden sollen
pars = list(idi = tasks$task_id[1:2], nodesize = c(0.0000001, 1:10)/(10*4)) # nodesize = 1 und dann in regelmäßigen Abständen relativ zu n
parset.design = data.frame()
task.design = makeDesign("taski", design = parset.design)


#task.design = makeDesign("taski", exhaustive = pars)

static = tasks

# pars = list(ntree = 10000)
# forest.design.mtry = makeDesign("forest.mtry", exhaustive = pars)
pars = list(num.trees = 10000)
forest.design.nodesize = makeDesign("forest.nodesize", exhaustive = pars)

addExperiments(regis, repls = 30, prob.designs = task.design, algo.designs = list(forest.design.ntree)) # 12.5 h 
addExperiments(regis, repls = 2, prob.designs = task.design, algo.designs = list(forest.design.nodesize)) # 16.5 h


summarizeExperiments(regis)
testJob(regis)

# Chunk jobs
id1 = findExperiments(regis, ...)
submitJobs(regis, ids = )
#waitForJobs(regis)

#regis = loadRegistry("/home/probst/Random_Forest/RFParset/results/par_randomForest-files")
#showStatus(regis)

#res = loadResults(regis)
#lapply(res, `[[`, 1)

